#pragma once
void caculate (const float *c_freq, const int *c_isingl, const int *c_nimage, const int *c_ibwin, const float *c_deltas, const int *c_maxn, const float *c_zbox, const float *c_rbox, const float *c_epmult, const float *c_rloop, const char *c_topopt, const float *c_deptht, double *cpt_real, double *cpt_aimag, const double *c_rhot, const char *c_botopt, const float *c_depthb, double *cpb_real, double *cpb_aimag, const double *c_rhob, const char *c_runtype, const char *c_beamtype, void *c_line_length, void *c_xv_result);
void delete_c_chars ();
void delete_c_line_length (int *c_line_length /* WARNING: non-interoperable KIND */ );
void delete_growth_double_vector (double *vector_ptr /* WARNING: non-interoperable KIND */ );
void readconfig (void *c_title_pointer, float *c_freq, int *c_isingl, int *c_nimage, int *c_ibwin, float *c_deltas, int *c_maxn, float *c_zbox, float *c_rbox, float *c_epmult, float *c_rloop, void *c_topopt_pointer, float *c_deptht, double *cpt_real, double *cpt_aimag, double *c_rhot, void *c_botopt_pointer, float *c_depthb, double *cpb_real, double *cpb_aimag, double *c_rhob, void *c_runtype_pointer, void *c_beamtype_pointer);
