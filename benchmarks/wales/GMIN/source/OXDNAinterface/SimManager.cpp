/*
 * SimManager.cpp
 *
 *  Created on: 03/set/2010
 *      Author: lorenzo
 */

#include <csignal>
#include "SimManager.h"
#ifndef NOCUDA
#include "CUDA/CUDANoList.h"
#include "CUDA/CUDASimpleVerletList.h"
#endif

void gbl_terminate (int arg) {
	fprintf (stderr, "# Caught SIGNAL %d; setting stop = 1\n", arg);
	SimManager::stop = true;
}

bool SimManager::stop = false;

SimManager::SimManager(IOManager &IO, const char *input_file) :
	_IO(IO), _print_reduced_conf_every(0), _print_energy_every(1000), _restart_step_counter(false) {
	_start_step = _cur_step = _steps = 0;
	_IO = IO;

	// variable to terminate the program; it is checked in the main
	// loop; if set to 1, the mayn cycle will exit;
	//void (*termhandler)(int);

	loadInputFile(&_input, input_file);
	if(_input.state == ERROR) _IO.die("Caught an error while opening the input file");

	char backend_opt[256], backend_prec[256], sim_type[256];
	getInputString(&_input, "backend", backend_opt, 1);

	if(getInputString(&_input, "backend_precision", backend_prec, 0) == KEY_NOT_FOUND) {
		_IO.log(_IO.LOG_INFO, "Backend precision not speficied, using double");
		sprintf(backend_prec, "%s", "double");
	}
	else _IO.log(_IO.LOG_INFO, "Backend precision: %s", backend_prec);

	if(getInputString(&_input, "sim_type", sim_type, 0) == KEY_NOT_FOUND) {
		_IO.log(_IO.LOG_INFO, "Simulation type not specified, using MD");
		sprintf(sim_type, "%s", "MD");
	}
	else _IO.log(_IO.LOG_INFO, "Simulation type: %s", sim_type);

	// I know that this is really ugly but I couldn't find a better way
	if(!strcmp(sim_type, "MD")) {
		if(!strcmp(backend_opt, "CPU")) {
			if(!strcmp(backend_prec, "double")) _backend = new MD_CPUBackend<double>(&_IO);
			else if(!strcmp(backend_prec, "float")) _backend = new MD_CPUBackend<float>(&_IO);
			else _IO.die("Backend precision '%s' is not supported", backend_prec);
		}
#ifndef NOCUDA
		else if(!strcmp(backend_opt, "CUDA")) {
			// what's the list type we want to use?
			char list_type[256];

			// LR_double4 is defined in CUDAUtils.h
			if(!strcmp(backend_prec, "double")) {
				if(getInputString(&_input, "CUDA_list", list_type, 0) == KEY_NOT_FOUND || !strcmp("no", list_type))
					_backend = new MD_CUDABackend<double, LR_double4, CUDANoList<double, LR_double4> >(&_IO);
				else if(!strcmp("verlet", list_type))
					_backend = new MD_CUDABackend<double, LR_double4, CUDASimpleVerletList<double, LR_double4> >(&_IO);
				else _IO.die("CUDA_list '%s' is not supported", list_type);
			}
			else if(!strcmp(backend_prec, "float")) {
				if(getInputString(&_input, "CUDA_list", list_type, 0) == KEY_NOT_FOUND || !strcmp("no", list_type))
					_backend = new MD_CUDABackend<float, float4, CUDANoList<float, float4> >(&_IO);
				else if(!strcmp("verlet", list_type))
					_backend = new MD_CUDABackend<float, float4, CUDASimpleVerletList<float, float4> >(&_IO);
				else _IO.die("CUDA_list '%s' is not supported", list_type);
			}
			else if(!strcmp(backend_prec, "mixed")) {
				if(getInputString(&_input, "CUDA_list", list_type, 0) == KEY_NOT_FOUND || !strcmp("no", list_type))
					_backend = new CUDAMixedBackend<CUDANoList<float, float4> >(&_IO);
				else if(!strcmp("verlet", list_type))
					_backend = new CUDAMixedBackend<CUDASimpleVerletList<float, float4> >(&_IO);
				else _IO.die("CUDA_list '%s' is not supported", list_type);
			}
			else _IO.die("Backend precision '%s' is not supported", backend_prec);
		}
#endif
		else _IO.die("Backend '%s' not supported", backend_opt);
	}
	else if(!strcmp(sim_type, "MC")) {
		if(!strcmp(backend_opt, "CPU")) {
			if(!strcmp(backend_prec, "double")) _backend = new MC_CPUBackend<double>(&_IO);
			else if(!strcmp(backend_prec, "float")) _backend = new MC_CPUBackend<float>(&_IO);
			else _IO.die("Backend precision '%s' is not supported", backend_prec);
		}
#ifndef NOCUDA
		else if(!strcmp(backend_opt, "CUDA")) {
			// what's the list type we want to use?
			char list_type[256];

			// LR_double4 is defined in CUDAUtils.h
			if(!strcmp(backend_prec, "double")) {
				if(getInputString(&_input, "CUDA_list", list_type, 0) == KEY_NOT_FOUND || !strcmp("no", list_type))
					_backend = new MC_CUDABackend<double, LR_double4, CUDANoList<double, LR_double4> >(&_IO);
				else if(!strcmp("verlet", list_type))
					_backend = new MC_CUDABackend<double, LR_double4, CUDASimpleVerletList<double, LR_double4> >(&_IO);
				else _IO.die("CUDA_list '%s' is not supported", list_type);
			}
			else if(!strcmp(backend_prec, "float")) {
				if(getInputString(&_input, "CUDA_list", list_type, 0) == KEY_NOT_FOUND || !strcmp("no", list_type))
					_backend = new MC_CUDABackend<float, float4, CUDANoList<float, float4> >(&_IO);
				else if(!strcmp("verlet", list_type))
					_backend = new MC_CUDABackend<float, float4, CUDASimpleVerletList<float, float4> >(&_IO);
				else _IO.die("CUDA_list '%s' is not supported", list_type);
			}
			else _IO.die("Backend precision '%s' is not supported", backend_prec);
		}
#endif
		else _IO.die("Backend '%s' not supported", backend_opt);
	}
	else _IO.die("Simulation type '%s' not supported", sim_type);
}

SimManager::~SimManager() {
	cleanTimeScale(&_time_scale_manager);

	int updated = _backend->get_N_updates();
	if(updated > 0) _IO.log(_IO.LOG_INFO, "Lists updated %d times (every ~%lf steps)", updated, _steps / (double)updated);

	delete _backend;

	_IO.log(_IO.LOG_INFO, "END OF THE SIMULATION, everything went OK!");
}

void SimManager::_get_options() {
	getInputInt(&_input, "print_reduced_conf_every", &_print_reduced_conf_every, 0);
	getInputInt(&_input, "print_energy_every", &_print_energy_every, 0);
	getInputInt(&_input, "restart_step_counter", &_restart_step_counter, 0);
	getInputLLInt(&_input, "steps", &_steps, 1);
	if(getInputInt(&_input, "seed", &_seed, 0) == KEY_NOT_FOUND) _seed = time(NULL);
	getInputString(&_input, "conf_file", _conf_file, 1);
}

void SimManager::load_options() {
	_get_options();

	_IO.get_settings(_input);
	_backend->get_settings(_input);
}

void SimManager::init() {
	srand48(_seed);
	srand(_seed);

	ifstream conf_input(_conf_file);
	if(conf_input.good() == false) _IO.die("Can't read configuration file '%s'", _conf_file);

	// here we handle the SIGTERM signal;
	signal (SIGTERM, gbl_terminate);
	signal (SIGABRT, gbl_terminate);
	signal (SIGINT, gbl_terminate);

	char time_scale[256];
	getInputString(&_input, "time_scale", time_scale, 1);
	if(strcmp(time_scale, "linear") == 0) _time_scale = TS_LIN;
	else if(strcmp(time_scale, "log_lin") == 0) _time_scale = TS_LOG_LIN;
	else _IO.die("Time scale '%s' not supported", time_scale);

	char line[512];
	conf_input.getline(line, 512);
	int res = sscanf(line, "t = %lld", &_start_step);
	if(res != 1) _IO.die("The first line of the configuration file is not correct, aborting");
	if(_restart_step_counter != 0) _start_step = 0;

	// init time_scale_manager
	initTimeScale(&_time_scale_manager, _time_scale);

	int tmp;
	getInputInt(&_input, "print_conf_interval", &tmp, 1);
	setTSInterval(&_time_scale_manager, tmp);

	if(_time_scale == TS_LOG_LIN) {
		getInputInt(&_input, "print_conf_ppc", &tmp, 1);
		setTSPPC(&_time_scale_manager, tmp);
	}
	// end

	setTSInitialStep(&_time_scale_manager, _start_step);

	_IO.init();
	_backend->init(conf_input);

	_max_steps = _start_step + _steps;
}

void SimManager::run() {
	// main loop
	for(_cur_step = _start_step; _cur_step < _max_steps && !SimManager::stop; _cur_step++) {
		_backend->sim_step(_cur_step);

		if(_print_energy_every > 0 && (_cur_step % _print_energy_every) == 0) {
			_backend->print_energy(_cur_step);
			_backend->print_conf(_cur_step, false, true);
		}
		if(_cur_step == _time_scale_manager.next_step) {
			_backend->print_conf(_cur_step);
			setTSNextStep(&_time_scale_manager);
		}
		if(_print_reduced_conf_every > 0 && _cur_step > 0 && (_cur_step % _print_reduced_conf_every) == 0) _backend->print_conf(_cur_step, true);
	}

	if(_print_energy_every > 0) _backend->print_energy(_cur_step);
	_backend->print_conf(_cur_step, false, true);
}

void SimManager::clean() {
	char unread[1024];
	unread[0] = '\0';

	// print unread (i.e. unused) keys
	setUnreadKeys(&_input);
	if(_input.N_unread_keys > 0) {
		for(int i = 0; i < _input.N_unread_keys; i++) {
			if(strlen(unread) < 1024)
				sprintf(unread+strlen(unread), "\n\t%s", _input.unread_keys[i]);
		}
		_IO.debug("The following keys found in the input file were not used: %s", unread);
	}
	cleanInputFile(&_input);
}
