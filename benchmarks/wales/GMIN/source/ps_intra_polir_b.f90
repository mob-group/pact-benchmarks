!*** PARAMETERS USED FOR THE CALCULATION OF THE POTENTIAL ENERGY AND THE
!*** DIPOLE MOMENT OF THE WATER MONOMER. TAKEN FROM:
!*** "H. PARTRIDGE AND D. W. SCHWENKE, J. CHEM. PHYS. 106, 4618 (1997)"
        MODULE MNASA_MOD2
IMPLICIT NONE
INTEGER, DIMENSION(245,3) :: IDX
INTEGER, DIMENSION(84 ,3) :: IDXD
DOUBLE PRECISION, DIMENSION(84) :: COEFD
DOUBLE PRECISION, DIMENSION(245) :: C5ZA, CBASIS, CCORE, CREST
INTEGER, DIMENSION(9,3) :: IDXM
DOUBLE PRECISION, DIMENSION(9) :: CMASS
DOUBLE PRECISION :: REOH,THETAE,B1,ROH,ALPHAOH,DEOHA,PHH1A,PHH2
DOUBLE PRECISION :: F5Z,FBASIS,FCORE,FREST
DOUBLE PRECISION :: A,B,C0,C1,C2,B1D
DATA IDX(:,1)/  &
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2,  &
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,  &
      2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,  &
      3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3,  &
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,  &
      4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4,  &
      4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6,  &
      6, 6, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5,  &
      6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 5, 5,  &
      5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7,  &
      7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6,  &
      6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 9, 9,  &
      9, 9, 9, 9, 9/
DATA IDX(:,2)/ &
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  &
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,  &
      2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2,  &
      2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3,  &
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,  &
      2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3,  &
      3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,  &
      1, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3,  &
      2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4,  &
      4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2,  &
      2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4,  &
      4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 1, 1,  &
      1, 1, 1, 1, 1/
DATA IDX(:,3)/ &
      1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15, 1, 2, 3, 4, 5,  &
      6, 7, 8, 9,10,11,12,13,14, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,  &
     12,13, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, 1, 2, 3, 4, 5,  &
      6, 7, 8, 9,10,11,12, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, 1,  &
      2, 3, 4, 5, 6, 7, 8, 9,10,11, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,  &
     11, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11, 1, 2, 3, 4, 5, 6, 7, 8,  &
      9,10, 1, 2, 3, 4, 5, 6, 7, 8, 9,10, 1, 2, 3, 4, 5, 6, 7, 8,  &
      9,10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9,  &
      1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2,  &
      3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6,  &
      7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 1, 2, 3,  &
      4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7, 1, 2,  &
      3, 4, 5, 6, 7/
DATA C5ZA(1:245)/  &
      4.2278462684916D+04, 4.5859382909906D-02, 9.4804986183058D+03, &
      7.5485566680955D+02, 1.9865052511496D+03, 4.3768071560862D+02, &
      1.4466054104131D+03, 1.3591924557890D+02,-1.4299027252645D+03, &
      6.6966329416373D+02, 3.8065088734195D+03,-5.0582552618154D+02, &
     -3.2067534385604D+03, 6.9673382568135D+02, 1.6789085874578D+03, &
     -3.5387509130093D+03,-1.2902326455736D+04,-6.4271125232353D+03, &
     -6.9346876863641D+03,-4.9765266152649D+02,-3.4380943579627D+03, &
      3.9925274973255D+03,-1.2703668547457D+04,-1.5831591056092D+04, &
      2.9431777405339D+04, 2.5071411925779D+04,-4.8518811956397D+04, &
     -1.4430705306580D+04, 2.5844109323395D+04,-2.3371683301770D+03, &
      1.2333872678202D+04, 6.6525207018832D+03,-2.0884209672231D+03, &
     -6.3008463062877D+03, 4.2548148298119D+04, 2.1561445953347D+04, &
     -1.5517277060400D+05, 2.9277086555691D+04, 2.6154026873478D+05, &
     -1.3093666159230D+05,-1.6260425387088D+05, 1.2311652217133D+05, &
     -5.1764697159603D+04, 2.5287599662992D+03, 3.0114701659513D+04, &
     -2.0580084492150D+03, 3.3617940269402D+04, 1.3503379582016D+04, &
     -1.0401149481887D+05,-6.3248258344140D+04, 2.4576697811922D+05, &
      8.9685253338525D+04,-2.3910076031416D+05,-6.5265145723160D+04, &
      8.9184290973880D+04,-8.0850272976101D+03,-3.1054961140464D+04, &
     -1.3684354599285D+04, 9.3754012976495D+03,-7.4676475789329D+04, &
     -1.8122270942076D+05, 2.6987309391410D+05, 4.0582251904706D+05, &
     -4.7103517814752D+05,-3.6115503974010D+05, 3.2284775325099D+05, &
      1.3264691929787D+04, 1.8025253924335D+05,-1.2235925565102D+04, &
     -9.1363898120735D+03,-4.1294242946858D+04,-3.4995730900098D+04, &
      3.1769893347165D+05, 2.8395605362570D+05,-1.0784536354219D+06, &
     -5.9451106980882D+05, 1.5215430060937D+06, 4.5943167339298D+05, &
     -7.9957883936866D+05,-9.2432840622294D+04, 5.5825423140341D+03, &
      3.0673594098716D+03, 8.7439532014842D+04, 1.9113438435651D+05, &
     -3.4306742659939D+05,-3.0711488132651D+05, 6.2118702580693D+05, &
     -1.5805976377422D+04,-4.2038045404190D+05, 3.4847108834282D+05, &
     -1.3486811106770D+04, 3.1256632170871D+04, 5.3344700235019D+03, &
      2.6384242145376D+04, 1.2917121516510D+05,-1.3160848301195D+05, &
     -4.5853998051192D+05, 3.5760105069089D+05, 6.4570143281747D+05, &
     -3.6980075904167D+05,-3.2941029518332D+05,-3.5042507366553D+05, &
      2.1513919629391D+03, 6.3403845616538D+04, 6.2152822008047D+04, &
     -4.8805335375295D+05,-6.3261951398766D+05, 1.8433340786742D+06, &
      1.4650263449690D+06,-2.9204939728308D+06,-1.1011338105757D+06, &
      1.7270664922758D+06, 3.4925947462024D+05,-1.9526251371308D+04, &
     -3.2271030511683D+04,-3.7601575719875D+05, 1.8295007005531D+05, &
      1.5005699079799D+06,-1.2350076538617D+06,-1.8221938812193D+06, &
      1.5438780841786D+06,-3.2729150692367D+03, 1.0546285883943D+04, &
     -4.7118461673723D+04,-1.1458551385925D+05, 2.7704588008958D+05, &
      7.4145816862032D+05,-6.6864945408289D+05,-1.6992324545166D+06, &
      6.7487333473248D+05, 1.4361670430046D+06,-2.0837555267331D+05, &
      4.7678355561019D+05,-1.5194821786066D+04,-1.1987249931134D+05, &
      1.3007675671713D+05, 9.6641544907323D+05,-5.3379849922258D+05, &
     -2.4303858824867D+06, 1.5261649025605D+06, 2.0186755858342D+06, &
     -1.6429544469130D+06,-1.7921520714752D+04, 1.4125624734639D+04, &
     -2.5345006031695D+04, 1.7853375909076D+05,-5.4318156343922D+04, &
     -3.6889685715963D+05, 4.2449670705837D+05, 3.5020329799394D+05, &
      9.3825886484788D+03,-8.0012127425648D+05, 9.8554789856472D+04, &
      4.9210554266522D+05,-6.4038493953446D+05,-2.8398085766046D+06, &
      2.1390360019254D+06, 6.3452935017176D+06,-2.3677386290925D+06, &
     -3.9697874352050D+06,-1.9490691547041D+04, 4.4213579019433D+04, &
      1.6113884156437D+05,-7.1247665213713D+05,-1.1808376404616D+06, &
      3.0815171952564D+06, 1.3519809705593D+06,-3.4457898745450D+06, &
      2.0705775494050D+05,-4.3778169926622D+05, 8.7041260169714D+03, &
      1.8982512628535D+05,-2.9708215504578D+05,-8.8213012222074D+05, &
      8.6031109049755D+05, 1.0968800857081D+06,-1.0114716732602D+06, &
      1.9367263614108D+05, 2.8678295007137D+05,-9.4347729862989D+04, &
      4.4154039394108D+04, 5.3686756196439D+05, 1.7254041770855D+05, &
     -2.5310674462399D+06,-2.0381171865455D+06, 3.3780796258176D+06, &
      7.8836220768478D+05,-1.5307728782887D+05,-3.7573362053757D+05, &
      1.0124501604626D+06, 2.0929686545723D+06,-5.7305706586465D+06, &
     -2.6200352535413D+06, 7.1543745536691D+06,-1.9733601879064D+04, &
      8.5273008477607D+04, 6.1062454495045D+04,-2.2642508675984D+05, &
      2.4581653864150D+05,-9.0376851105383D+05,-4.4367930945690D+05, &
      1.5740351463593D+06, 2.4563041445249D+05,-3.4697646046367D+03, &
     -2.1391370322552D+05, 4.2358948404842D+05, 5.6270081955003D+05, &
     -8.5007851251980D+05,-6.1182429537130D+05, 5.6690751824341D+05, &
     -3.5617502919487D+05,-8.1875263381402D+02,-2.4506258140060D+05, &
      2.5830513731509D+05, 6.0646114465433D+05,-6.9676584616955D+05, &
      5.1937406389690D+05, 1.7261913546007D+05,-1.7405787307472D+04, &
     -3.8301842660567D+05, 5.4227693205154D+05, 2.5442083515211D+06, &
     -1.1837755702370D+06,-1.9381959088092D+06,-4.0642141553575D+05, &
      1.1840693827934D+04,-1.5334500255967D+05, 4.9098619510989D+05, &
      6.1688992640977D+05, 2.2351144690009D+05,-1.8550462739570D+06, &
      9.6815110649918D+03,-8.1526584681055D+04,-8.0810433155289D+04, &
      3.4520506615177D+05, 2.5509863381419D+05,-1.3331224992157D+05, &
     -4.3119301071653D+05,-5.9818343115856D+04, 1.7863692414573D+03, &
      8.9440694919836D+04,-2.5558967650731D+05,-2.2130423988459D+04, &
      4.4973674518316D+05,-2.2094939343618D+05/
!
!     EXPANSION COEFFICIENTS FOR BASIS CORRECTION
!
DATA CBASIS(1:245)/ &
      6.9770019624764D-04,-2.4209870001642D+01, 1.8113927151562D+01, &
      3.5107416275981D+01,-5.4600021126735D+00,-4.8731149608386D+01, &
      3.6007189184766D+01, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
     -7.7178474355102D+01,-3.8460795013977D+01,-4.6622480912340D+01, &
      5.5684951167513D+01, 1.2274939911242D+02,-1.4325154752086D+02, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00,-6.0800589055949D+00, &
      8.6171499453475D+01,-8.4066835441327D+01,-5.8228085624620D+01, &
      2.0237393793875D+02, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      3.3525582670313D+02, 7.0056962392208D+01,-4.5312502936708D+01, &
     -3.0441141194247D+02, 2.8111438108965D+02, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00,-1.2983583774779D+02, 3.9781671212935D+01, &
     -6.6793945229609D+01,-1.9259805675433D+02, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00,-8.2855757669957D+02,-5.7003072730941D+01, &
     -3.5604806670066D+01, 9.6277766002709D+01, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 8.8645622149112D+02,-7.6908409772041D+01, &
      6.8111763314154D+01, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      2.5090493428062D+02,-2.3622141780572D+02, 5.8155647658455D+02, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 2.8919570295095D+03, &
     -1.7871014635921D+02,-1.3515667622500D+02, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00,-3.6965613754734D+03, 2.1148158286617D+02, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00,-1.4795670139431D+03, &
      3.6210798138768D+02, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
     -5.3552886800881D+03, 3.1006384016202D+02, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 1.6241824368764D+03, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 4.3764909606382D+03, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 1.0940849243716D+03, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 3.0743267832931D+03, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00/
!
!     EXPANSION COEFFICIENTS FOR CORE CORRECTION
!
DATA CCORE(1:245)/ &
      2.4332191647159D-02,-2.9749090113656D+01, 1.8638980892831D+01, &
     -6.1272361746520D+00, 2.1567487597605D+00,-1.5552044084945D+01, &
      8.9752150543954D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
     -3.5693557878741D+02,-3.0398393196894D+00,-6.5936553294576D+00, &
      1.6056619388911D+01, 7.8061422868204D+01,-8.6270891686359D+01, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00,-3.1688002530217D+01, &
      3.7586725583944D+01,-3.2725765966657D+01,-5.6458213299259D+00, &
      2.1502613314595D+01, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      5.2789943583277D+02,-4.2461079404962D+00,-2.4937638543122D+01, &
     -1.1963809321312D+02, 2.0240663228078D+02, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00,-6.2574211352272D+02,-6.9617539465382D+00, &
     -5.9440243471241D+01, 1.4944220180218D+01, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00,-1.2851139918332D+03,-6.5043516710835D+00, &
      4.0410829440249D+01,-6.7162452402027D+01, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 1.0031942127832D+03, 7.6137226541944D+01, &
     -2.7279242226902D+01, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
     -3.3059000871075D+01, 2.4384498749480D+01,-1.4597931874215D+02, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 1.6559579606045D+03, &
      1.5038996611400D+02,-7.3865347730818D+01, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00,-1.9738401290808D+03,-1.4149993809415D+02, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00,-1.2756627454888D+02, &
      4.1487702227579D+01, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
     -1.7406770966429D+03,-9.3812204399266D+01, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00,-1.1890301282216D+03, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 2.3723447727360D+03, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00,-1.0279968223292D+03, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 5.7153838472603D+02, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00/
!
!     EXPANSION COEFFICIENTS FOR V REST
!
DATA CREST(1:245)/ &
      0.0000000000000D+00,-4.7430930170000D+00,-1.4422132560000D+01, &
     -1.8061146510000D+01, 7.5186735000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
     -2.7962099800000D+02, 1.7616414260000D+01,-9.9741392630000D+01, &
      7.1402447000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00,-7.8571336480000D+01, &
      5.2434353250000D+01, 7.7696745000000D+01, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      1.7799123760000D+02, 1.4564532380000D+02, 2.2347226000000D+02, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00,-4.3823284100000D+02,-7.2846553000000D+02, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00,-2.6752313750000D+02, 3.6170310000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00, 0.0000000000000D+00, &
      0.0000000000000D+00, 0.0000000000000D+00/
!
!     EXPANSION INDICIES FOR MASS CORRECTION
!
      DATA IDXM/1,2,1,1,3,2,1,2,1, &
                2,1,1,3,1,2,2,1,1, &
                1,1,2,1,1,1,2,2,3/
!
!     EXPANSION COEFFICIENTS FOR MASS CORRECTION
!
      DATA CMASS/ -8.3554183D+00,3.7036552D+01,-5.2722136D+00, &
           1.6843857D+01,-7.0929741D+01,5.5380337D+00,-2.9962997D+01, &
           1.3637682D+02,-3.0530195D+00/
!
!     TWO BODY PARAMETERS
!
      DATA REOH,THETAE,B1,ROH,ALPHAOH,DEOHA,PHH1A,PHH2/0.958649D0, &
           104.3475D0,2.0D0,0.9519607159623009D0,2.587949757553683D0, &
           42290.92019288289D0,16.94879431193463D0,12.66426998162947D0/
!
!     SCALING FACTORS FOR CONTRIBUTIONS TO EMPERICAL POTENTIAL
!
      DATA F5Z,FBASIS,FCORE,FREST/0.99967788500000D0, &
          0.15860145369897D0,-1.6351695982132D0,1D0/

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
DATA IDXD(1:84,1)/ &
      1, 1, 1, 2, 1, 1, 1, 2, 2, 3, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, &
      1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5, 1, 1, 1, 1, 1, &
      1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 1, 1, 1, 1, &
      1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, &
      5, 6, 6, 7/
DATA IDXD(1:84,2)/ &
      1, 1, 2, 1, 1, 2, 3, 1, 2, 1, 1, 2, 3, 4, 1, 2, 3, 1, 2, 1, &
      1, 2, 3, 4, 5, 1, 2, 3, 4, 1, 2, 3, 1, 2, 1, 1, 2, 3, 4, 5, &
      6, 1, 2, 3, 4, 5, 1, 2, 3, 4, 1, 2, 3, 1, 2, 1, 1, 2, 3, 4, &
      5, 6, 7, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 1, 2, 3, 4, 1, 2, &
      3, 1, 2, 1/
DATA IDXD(1:84,3)/ &
      1, 2, 1, 1, 3, 2, 1, 2, 1, 1, 4, 3, 2, 1, 3, 2, 1, 2, 1, 1, &
      5, 4, 3, 2, 1, 4, 3, 2, 1, 3, 2, 1, 2, 1, 1, 6, 5, 4, 3, 2, &
      1, 5, 4, 3, 2, 1, 4, 3, 2, 1, 3, 2, 1, 2, 1, 1, 7, 6, 5, 4, &
      3, 2, 1, 6, 5, 4, 3, 2, 1, 5, 4, 3, 2, 1, 4, 3, 2, 1, 3, 2, &
      1, 2, 1, 1/
DATA COEFD(1:84) /&
     -2.1689686086730D-03, 1.4910379754728D-02, 5.3546078430060D-02, &
     -7.4055995388666D-02,-3.7764333017616D-03, 1.4089887256484D-01, &
     -6.2584207687264D-02,-1.1260393113022D-01,-5.7824159269319D-02, &
      1.4360743650655D-02,-1.5469680141070D-02,-1.3036350092795D-02, &
      2.7515837781556D-02, 1.4098478875076D-01,-2.7663168397781D-02, &
     -5.2378176254797D-03,-1.0237198381792D-02, 8.9571999265473D-02, &
      7.2920263098603D-03,-2.6873260551686D-01, 2.0220870325864D-02, &
     -7.0764766270927D-02, 1.2140640273760D-01, 2.0978491966341D-02, &
     -1.9443840512668D-01, 4.0826835370618D-02,-4.5365190474650D-02, &
      6.2779900072132D-02,-1.3194351021000D-01,-1.4673032718563D-01, &
      1.1894031277247D-01,-6.4952851564679D-03, 8.8503610374493D-02, &
      1.4899437409291D-01, 1.3962841511565D-01,-2.6459446720450D-02, &
     -5.0128914532773D-02, 1.8329676428116D-01,-1.5559089125095D-01, &
     -4.0176879767592D-02, 3.6192059996636D-01, 1.0202887240343D-01, &
      1.9318668580051D-01,-4.3435977107932D-01,-4.2080828803311D-02, &
      1.9144626027273D-01,-1.7851138969948D-01, 1.0524533875070D-01, &
     -1.7954071602185D-02, 5.2022455612120D-02,-2.8891891146828D-01, &
     -4.7452036576319D-02,-1.0939400546289D-01, 3.5916564473568D-01, &
     -2.0162789820172D-01,-3.5838629543696D-01, 5.6706523551202D-03, &
      1.3849337488211D-01,-4.1733982195604D-01, 4.1641570764241D-01, &
     -1.2243429796296D-01, 4.7141730971228D-02,-1.8224510249551D-01, &
     -1.8880981556620D-01,-3.1992359561800D-01,-1.8567550546587D-01, &
      6.1850530431280D-01,-6.1142756235141D-02,-1.6996135584933D-01, &
      5.4252879499871D-01, 6.6128603899427D-01, 1.2107016404639D-02, &
     -1.9633639729189D-01, 2.7652059420824D-03,-2.2684111109778D-01, &
     -4.7924491598635D-01, 2.4287790137314D-01,-1.4296023329441D-01, &
      8.9664665907006D-02,-1.4003228575602D-01,-1.3321543452254D-01,&
     -1.8340983193745D-01, 2.3426707273520D-01, 1.5141050914514D-01/
DATA B1D/1.D0/
DATA A,B,C0,C1,C2/0.2999D0,-0.6932D0,1.0099D0,-0.1801D0,0.0892D0/

CONTAINS

SUBROUTINE POT_NASA(R1, DR1, E1)
!****
!**** CALCULATES THE ENERGY (E1) AND THE DERIVATIVES (DR1) OF A WATER MONOMER
!**** (COORDINATES IN THE ARRAY R1). THE POTENTIAL HAS BEEN DEVELOPED BY
!**** "H. PARTRIDGE AND D. W. SCHWENKE, J. CHEM. PHYS. 106, 4618 (1997)".
!**** SOME EXTRA CODE FOR THE CALCULATION OF THE ENERGY DERIVATIVES HAS BEEN
!**** ADDED  BY C. J. BURNHAM.
!****
IMPLICIT NONE
DOUBLE PRECISION, DIMENSION(3, 3), INTENT(IN) :: R1
DOUBLE PRECISION, DIMENSION(3, 3), INTENT(OUT) :: DR1
DOUBLE PRECISION, INTENT(OUT) :: E1
DOUBLE PRECISION, DIMENSION(3) :: ROH1, ROH2, RHH
DOUBLE PRECISION, DIMENSION(0:15,3) :: FMAT
DOUBLE PRECISION :: DROH1, DROH2, DROH1OH2, DRHH
DOUBLE PRECISION :: VA, VB, VC
DOUBLE PRECISION :: DVA1,DVA2,DVB, DVCDR1, DVCDR2, DVCDCTH, SUM0,SUM1,SUM2,SUM3
DOUBLE PRECISION :: X1, X2, X3
DOUBLE PRECISION :: COSTHE, COSTH, SINTH
DOUBLE PRECISION :: EXP1, EXP2, EFAC
INTEGER          :: I, J, INI, INJ, INK, IX, IY
DOUBLE PRECISION :: DEOH, PHH1
DOUBLE PRECISION, DIMENSION(245) :: C5Z
DOUBLE PRECISION :: P1, P2, PL1, PL2, PC0

ROH1(:) = R1(1:3, 2) - R1(1:3,1)
ROH2(:) = R1(1:3, 3) - R1(1:3,1)
RHH(:)  = R1(1:3, 2) - R1(1:3,3)
DROH1 = DSQRT(DOT_PRODUCT(ROH1, ROH1))
DROH2 = DSQRT(DOT_PRODUCT(ROH2, ROH2))
DRHH  = DSQRT(DOT_PRODUCT(RHH , RHH ))
COSTH = (ROH1(1)*ROH2(1) + ROH1(2)*ROH2(2) + ROH1(3)*ROH2(3) ) / (DROH1*DROH2)


C5Z = F5Z*C5ZA + FBASIS*CBASIS + FCORE*CCORE + FREST*CREST
DEOH = F5Z*DEOHA
PHH1 = F5Z*PHH1A
PHH1 = PHH1*DEXP(PHH2)

COSTHE = -.24780227221366464506D0

EXP1 = DEXP(-ALPHAOH*(DROH1-ROH))
EXP2 = DEXP(-ALPHAOH*(DROH2-ROH))
VA = DEOH*(EXP1*(EXP1-2.D0)+EXP2*(EXP2-2.D0))
VB  = PHH1*DEXP(-PHH2*DRHH)
DVA1= 2.D0*ALPHAOH*DEOH*EXP1*(1.D0-EXP1)/DROH1
DVA2= 2.D0*ALPHAOH*DEOH*EXP2*(1.D0-EXP2)/DROH2
DVB = -PHH2*VB/DRHH
X1 = (DROH1-REOH)/REOH
X2 = (DROH2-REOH)/REOH
X3 = COSTH - COSTHE
FMAT(0,1:3) = 0.D0
FMAT(1,1:3) = 1.D0
DO J=2,15
  FMAT(J,1) = FMAT(J-1,1)*X1
  FMAT(J,2) = FMAT(J-1,2)*X2
  FMAT(J,3) = FMAT(J-1,3)*X3
ENDDO

EFAC = DEXP(-B1*(  (DROH1-REOH)**2 + (DROH2-REOH)**2))

SUM0 = 0.D0; SUM1 = 0.D0; SUM2 = 0.D0; SUM3 = 0.D0
DO J=2,245
  INI = IDX(J,1)
  INJ = IDX(J,2)
  INK = IDX(J,3)
  SUM0 = SUM0 + C5Z(J) *  ( FMAT(INI,1)*FMAT(INJ,2) +   &
       FMAT(INJ,1)*FMAT(INI,2)) *FMAT(INK,3)
  SUM1 = SUM1 + C5Z(J) *  ( DBLE(INI-1)*FMAT(INI-1,1)*FMAT(INJ,2) +   &
       DBLE(INJ-1)*FMAT(INJ-1,1)*FMAT(INI,2)  )*FMAT(INK,3)
  SUM2 = SUM2 + C5Z(J) *  ( DBLE(INJ-1)*FMAT(INI,1)*FMAT(INJ-1,2) +   &
       DBLE(INI-1)*FMAT(INJ,1)*FMAT(INI-1,2)  )*FMAT(INK,3)
  SUM3 = SUM3 + C5Z(J) *  ( FMAT(INI,1)*FMAT(INJ,2) +   &
       FMAT(INJ,1)*FMAT(INI,2)) * DBLE(INK-1)*FMAT(INK-1,3)
ENDDO

!.... ENERGY..........
VC= 2.D0*C5Z(1)+EFAC*SUM0
E1 = VA+VB+VC
E1 = E1+0.44739574026257D0! CORRECTION
E1 = E1*0.00285914375100642899D0 ! CM-1 --> KCAL/MOL
!.... DERIVATIVES .............
DVCDR1 = (-2.D0*B1*EFAC*(DROH1-REOH)*SUM0 + EFAC*SUM1/REOH)/DROH1
DVCDR2 = (-2.D0*B1*EFAC*(DROH2-REOH)*SUM0 + EFAC*SUM2/REOH)/DROH2
DVCDCTH = EFAC*SUM3

DR1(:,2) = DVA1*ROH1 + DVB*RHH + DVCDR1*ROH1 + &
      DVCDCTH*(ROH2(:)/(DROH1*DROH2)-COSTH*ROH1(:)/(DROH1*DROH1))
DR1(:,3) = DVA2*ROH2-DVB*RHH +DVCDR2*ROH2  + &
      DVCDCTH*(ROH1(:)/(DROH1*DROH2)-COSTH*ROH2(:)/(DROH2*DROH2))
DR1(:,1) = -(DR1(:,2)+DR1(:,3))
DR1 = DR1*(.00285914375100642899D0)
END SUBROUTINE POT_NASA

END MODULE MNASA_MOD2
