#include <Rcpp.h>
// [[Rcpp::depends(BH)]]
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/classification.hpp>
using namespace boost::algorithm;

// [[Rcpp::export]]
std::vector < std::string > lang_project(std::vector < std::string > urls, std::vector < std::string > subdomains){
  
  std::vector < std::string > holding;
  for(unsigned int i = 0; i <  urls.size(); ++i){
    split(holding, urls[i], is_any_of("."));
    if(std::find(subdomains.begin(), subdomains.end(), urls[1]) == subdomains.end()){
      urls[i] = holding[0] + "." + holding[1];
    } else {
      urls[i] = holding[0] + "." + holding[2];
    }
  }
  
  return urls;
}
