#pragma once
void caculate (const float *c_freq, const int *c_isingl, const int *c_nimage, const int *c_ibwin, const float *c_deltas, const int *c_maxn, const float *c_zbox, const float *c_rbox, const float *c_epmult, const float *c_rloop, const char *c_topopt, const float *c_deptht, double *cpt_real, double *cpt_aimag, const double *c_rhot, const char *c_botopt, const float *c_depthb, double *cpb_real, double *cpb_aimag, const double *c_rhob, const char *c_runtype, const char *c_beamtype, void *c_line_length, void *c_xv_result);
void ccaculate (const void *c_title, const int *c_title_len /* WARNING: non-interoperable KIND */ , const float *c_freq, const int *c_isingl, const int *c_nimage, const int *c_ibwin, const float *c_deltas, const float *c_zbox, const float *c_rbox, const float *c_epmult, const float *c_rloop, const char *c_topopt, const char *c_botopt, const float *c_depthb, const char *c_runtype, const char *c_beamtype, void *c_line_length, void *c_xv_result, const int *c_nmedia, const void *c_zsspv, const void *c_csspv, const void *c_sd, const void *c_rd, const void *c_r, const void *c_alpha, const int *c_zsspv_len /* WARNING: non-interoperable KIND */ , const int *c_csspv_len /* WARNING: non-interoperable KIND */ , const int *c_sd_len /* WARNING: non-interoperable KIND */ , const int *c_nsd /* WARNING: non-interoperable KIND */ , const int *c_rd_len /* WARNING: non-interoperable KIND */ , const int *c_nrd /* WARNING: non-interoperable KIND */ , const int *c_r_len /* WARNING: non-interoperable KIND */ , const int *c_nr /* WARNING: non-interoperable KIND */ , const int *c_alpha_len /* WARNING: non-interoperable KIND */ , const double *c_alphar, const double *c_betar, const double *c_rhor, const double *c_alphai, const double *c_betai, const float *c_bsigma, const float *c_tsigma, const int *c_npts, const int *c_nbeams, const int *c_nbtypts /* WARNING: non-interoperable KIND */ , const void *c_btypts, void *c_u_result, int *c_u_row, int *c_u_col, void *c_narr_1d, void *c_amparr_1d, void *c_phasearr_1d, void *c_delayarr_1d, void *c_srcanglearr_1d, void *c_recvanglearr_1d, void *c_ntopbncarr_1d, void *c_nbotbncarr_1d, int *c_arr_row, int *c_arr_col);
void delete_anglemod_config_para ();
void delete_arr_result ();
void delete_arrmod_config_para ();
void delete_bdrymod_config_para ();
void delete_c_chars ();
void delete_line_length ();
void delete_refcomod_config_para ();
void delete_sdrdrmod_config_para ();
void delete_u_1d ();
void delete_xv_result ();
void delte_beampatternmod_config_para ();
void readconfig (void *c_title_pointer, float *c_freq, int *c_isingl, int *c_nimage, int *c_ibwin, float *c_deltas, int *c_maxn, float *c_zbox, float *c_rbox, float *c_epmult, float *c_rloop, void *c_topopt_pointer, float *c_deptht, double *cpt_real, double *cpt_aimag, double *c_rhot, void *c_botopt_pointer, float *c_depthb, double *cpb_real, double *cpb_aimag, double *c_rhob, void *c_runtype_pointer, void *c_beamtype_pointer);
