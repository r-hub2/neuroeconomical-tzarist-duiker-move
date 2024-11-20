//#include <Rcpp.h>
//using namespace Rcpp;
//
//// currently not used calling C method directly seems quicker
//// [[Rcpp::export]]
//NumericVector llBGBvarRcpp(NumericMatrix sigm, NumericMatrix paraOrth){
//  Rcpp::NumericVector res=1;
//res=0;
//for(int i =0; i<sigm.nrow();i++)
//{
//  res[0]+= log(1/(2*3.141593*sqrt(sigm(i,0))*sqrt(sigm(i,1))))-0.5 * (pow(paraOrth(i,0),2)/sigm(i,0) + pow(paraOrth(i,1),2)/sigm(i,1));
//}
//return(res);
//}
