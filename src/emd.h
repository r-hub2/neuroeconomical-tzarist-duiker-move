#ifdef __cplusplus
extern "C" {
#endif
#ifndef EMD 
#define EMD 

void emdR(int *Pn, int *Qn, double *Px, double *Py, double *Pw, double *Qx, double *Qy, double *Qw, double *res, double *th, int *gc);

void emdR_gd(int *Pn, int *Qn, double *Px, double *Py, double *Pw, double *Qx, double *Qy, double *Qw, double *res, double *th, int *gc);

#endif
#ifdef __cplusplus
}
#endif
