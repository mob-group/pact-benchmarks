/*
 * SimManager.h
 *
 *  Created on: 03/set/2010
 *      Author: lorenzo
 */

#ifndef SIMMANAGER_H_
#define SIMMANAGER_H_

#include <cstring>
#include <ctime>

extern "C" {
#include "parse_input.h"
#include "timing.h"
#include "time_scales.h"
}

#include "defs.h"
#include "MD_CPUBackend.h"
#include "MC_CPUBackend.h"
#include "IOManager.h"

#ifndef NOCUDA
#include "CUDA/MD_CUDABackend.h"
#include "CUDA/MC_CUDABackend.h"
#endif

struct double4;
struct float4;

class SimManager {
protected:
	IOManager &_IO;
	ISimBackend *_backend;
	input_file _input;
	time_scale _time_scale_manager;
	int _time_scale;
	llint _steps;
	llint _cur_step;
	llint _start_step;
	llint _max_steps;
	int _seed;
	int _print_reduced_conf_every;
	int _print_energy_every;
	int _restart_step_counter;

	char _conf_file[256];
	virtual void _get_options();

public:
	SimManager(IOManager &IO, const char *input_file);
	virtual ~SimManager();

	static bool stop;
	virtual void load_options();
	virtual void init();
	virtual void run();
	virtual void clean();
};

void gbl_termhandler(int);

#endif /* SIMMANAGER_H_ */
