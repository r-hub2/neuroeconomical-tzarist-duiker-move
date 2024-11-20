#include <R.h>
#include <stdlib.h>
#include <math.h>
#include <vector>
#include <iostream>
#include <fstream>

#include "emd_hat.h"
#include "emd_hat_signatures_interface.h"
#include "emd_hat_impl.h"
#include "emd.h"
// #include "tictoc.hpp"
#define d2r (M_PI / 180.0)
#define radEarth 6378.137

//calculate haversine distance for linear distance
//http://stackoverflow.com/questions/365826/calculate-distance-between-2-gps-coordinates
double haversine_km(feature_tt *F1,feature_tt *F2)
{
    double dlong = (F2->X - F1->X) * d2r;
    double dlat = (F2->Y - F1->Y) * d2r;
    double a = std::min( pow(sin(dlat/2.0), 2) + cos(F1->Y*d2r) * cos(F2->Y*d2r) * pow(sin(dlong/2.0), 2), double (1));
    double c = 2 * atan2(sqrt(a), sqrt(1-a));
    double d = radEarth * c;

    return d;
}
double haversine_km_xy(double *X1, double *Y1, double *X2, double *Y2)
{
    double dlong = (*X2 - *X1) * d2r;
    double dlat = (*Y2 - *Y1) * d2r;
    double a = std::min(pow(sin(dlat/2.0), 2) + cos(*Y1*d2r) * cos(*Y2*d2r) * pow(sin(dlong/2.0), 2),double (1));
    double c = 2 * atan2(sqrt(a), sqrt(1-a));
    double d = radEarth * c;

    return d;
}

double eucl_dist_xy(double *X1, double *Y1, double *X2, double *Y2)
{
	double dX =*X1-*X2,dY=*Y1-*Y2;
	return sqrt(dX*dX+dY*dY);
}

double eucl_dist(feature_tt *F1, feature_tt *F2)
{
	double dX =F1->X-F2->X,dY=F1->Y-F2->Y;
	return sqrt(dX*dX+dY*dY);
}
extern "C" {
void emdR(int *Pn, int *Qn, double *Px, double *Py, double *Pw, double *Qx, double *Qy, double *Qw, double *res, double *th, int *gc)
{
    signature_tt<double> Psig2;
    signature_tt<double> Qsig2;
 Qsig2.n= *Qn;
 Psig2.n= *Pn;
    Psig2.Features= new feature_tt[Pn[0]];
    Qsig2.Features= new feature_tt[Qn[0]];
    Psig2.Weights= new double[Pn[0]];
    Qsig2.Weights= new double[Qn[0]];
    {for (signed int i=0; i<Qn[0]; ++i) {
        Qsig2.Features[i].X= Qx[i];
        Qsig2.Features[i].Y= Qy[i];
    }}
    Qsig2.Weights= Qw;
    {for (signed int i=0; i<Pn[0]; ++i) {
        Psig2.Features[i].X= Px[i];
        Psig2.Features[i].Y= Py[i];
    }}
    Psig2.Weights= Pw;
    double (*op)(feature_tt *F1,feature_tt *F2) = {NULL};
    if(*gc){
	op=&haversine_km;
    }else{
	op=&eucl_dist;
    }
    *res= emd_hat_signature_interface<double>(&Psig2, &Qsig2, *op,-1);
// Rprintf("Finished");
//    *res=eucl_dist(&Psig2.Features[0], &Psig2.Features[3]);
}

}
extern "C" {
void emdR_gd(int *Pn, int *Qn, double *Px, double *Py, double *Pw, double *Qx, double *Qy, double *Qw, double *res, double *th, int *gc)
{
    // tictoc timer;
    double THRESHOLD=*th;
    double (*op_xy)(double *X1, double *Y1, double *X2, double *Y2) = {NULL};
    if(*gc){
	op_xy=&haversine_km_xy;
    }else{
	op_xy=&eucl_dist_xy;
    }
//#### Define cost matrix ####
    std::vector< std::vector<double> > cost_mat; // for emd_hat version, defined globally only because of Rubner's version
    std::vector<double> cost_mat_row(*Pn);
    for (signed int i=0; i<*Qn; ++i) cost_mat.push_back(cost_mat_row);
    // timer.tic();
    std::vector<double> Qim,Pim;
    for (signed int i=0; i<*Pn; ++i) Pim.push_back(Pw[i]);
    for (signed int i=0; i<*Qn; ++i) Qim.push_back(Qw[i]);
    // timer.toc();
    // Rprintf("moving weights :%f \n",static_cast<double>(timer.totalTimeSec()));
    // timer.clear();
    // timer.tic();
    int max_cost_mat= -1;
    for (signed int j=0; j<*Qn; j++) {
            for (signed int i=0; i<*Pn; i++) {
                    cost_mat[i][j]= std::min(THRESHOLD,(op_xy(&Px[j],&Py[j],&Qx[i],&Qy[i])));
                    if (cost_mat[i][j]>max_cost_mat) max_cost_mat= cost_mat[i][j];
            }
    }
    // timer.clear();
    // timer.tic();
    *res= emd_hat_gd_metric<double>()(Qim,Pim, cost_mat,THRESHOLD);
    // timer.toc();
    // Rprintf("run emd :%f \n",static_cast<double>(timer.totalTimeSec()));
    // timer.clear();
}
}
