// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// Haversine distance (km) between two points (lon/lat degrees)
// [[Rcpp::export]]
NumericVector haversine_vec(NumericVector lon1, NumericVector lat1,
                            double lon2, double lat2) {
  int n = lon1.size();
  NumericVector out(n);
  const double R = 6371.0;
  double lon2r = lon2 * M_PI / 180.0;
  double lat2r = lat2 * M_PI / 180.0;
  for(int i=0;i<n;i++){
    double lon1r = lon1[i] * M_PI / 180.0;
    double lat1r = lat1[i] * M_PI / 180.0;
    double dl = lon2r - lon1r;
    double dt = lat2r - lat1r;
    double a = sin(dt/2.0)*sin(dt/2.0) + cos(lat1r)*cos(lat2r)*sin(dl/2.0)*sin(dl/2.0);
    double c = 2.0 * asin(std::min(1.0, sqrt(a)));
    out[i] = R * c;
  }
  return out;
}

// Nearest-grid index: find index of grid coordinate vector closest to value (argmin of squared diff)
// [[Rcpp::export]]
int nearest_index(NumericVector grid, double x) {
  int n = grid.size();
  double bestd = R_PosInf;
  int besti = 0;
  for(int i=0;i<n;i++){
    double d = (grid[i] - x);
    double dd = d*d;
    if(dd < bestd){ bestd = dd; besti = i; }
  }
  // return 0-based index for C++ convenience, convert to R 1-based if needed
  return besti;
}

// find_next_sst: expands box until it finds at least one cell with sst >= sstol
//
// sst_lon: length L lon vector
// sst_lat: length M lat vector
// sst_vals: numeric vector of length L*M representing matrix by column-major (as in R's as.vector(matrix))
//            i.e. positions correspond to expand.grid(sst_lon, sst_lat) ordering used in R code
// start_lon/start_lat: starting point
// expand_step: initial expand (degrees)
// max_iter: max expansions
// sstol: threshold
//
// returns numeric vector length 2: lon, lat of chosen cell (or input if none found)
// [[Rcpp::export]]
NumericVector find_next_sst_cpp(double start_lon, double start_lat,
                               NumericVector sst_lon, NumericVector sst_lat,
                               NumericVector sst_vals, double sstol,
                               double expand_step = 10.0, int max_iter = 20) {
  int L = sst_lon.size();
  int M = sst_lat.size();
  // Build arrays of lon/lat for the grid (as in expand.grid(sst_lon, sst_lat))
  // expand.grid(sst_lon, sst_lat) in R yields first varying sst_lon (fastest) then sst_lat,
  // but R's as.vector(matrix) depends on how matrix is built; earlier code used expand.grid then as.vector(sstmat$data[,,m]),
  // so sst_vals should match expand.grid ordering. We'll assume sst_vals length is L*M and its i-th entry corresponds to:
  // expand.grid(sst_lon, sst_lat)[i, ] i.e. grid_x = sst_lon[ ((i-1) %% L) ], grid_y = sst_lat[ ((i-1) / L) ] (0-based)
  // We'll generate coordinate vectors lazily when needed.

  NumericVector grid_lon(L * M);
  NumericVector grid_lat(L * M);
  int idx = 0;
  for(int j = 0; j < M; ++j){
    for(int i = 0; i < L; ++i){
      grid_lon[idx] = sst_lon[i];
      grid_lat[idx] = sst_lat[j];
      idx++;
    }
  }

  double expand = expand_step;
  for(int iter = 0; iter < max_iter; ++iter){
    double xmin = start_lon - expand;
    double xmax = start_lon + expand;
    double ymin = start_lat - expand;
    double ymax = start_lat + expand;
    // collect indices that fall inside and satisfy sst >= sstol
    std::vector<int> good_idx;
    good_idx.reserve(128);
    for(int k = 0; k < (L*M); ++k){
      double gx = grid_lon[k];
      double gy = grid_lat[k];
      if(gx >= xmin && gx <= xmax && gy >= ymin && gy <= ymax){
        if(sst_vals[k] >= sstol) good_idx.push_back(k);
      }
    }
    if(!good_idx.empty()){
      // compute distances to each candidate, pick nearest or within top 10% then randomize
      int ng = good_idx.size();
      NumericVector dists(ng);
      for(int a = 0; a < ng; ++a){
        int k = good_idx[a];
        // compute haversine between start and grid point
        double lon1r = start_lon * M_PI/180.0;
        double lat1r = start_lat * M_PI/180.0;
        double lon2r = grid_lon[k] * M_PI/180.0;
        double lat2r = grid_lat[k] * M_PI/180.0;
        double dl = lon2r - lon1r;
        double dt = lat2r - lat1r;
        double aa = sin(dt/2.0)*sin(dt/2.0) + cos(lat1r)*cos(lat2r)*sin(dl/2.0)*sin(dl/2.0);
        double cc = 2.0 * asin(std::min(1.0, sqrt(aa)));
        dists[a] = 6371.0 * cc;
      }
      // find order
      IntegerVector ord = seq_len(ng) - 1;
      std::sort(ord.begin(), ord.end(), [&](int a, int b){ return dists[a] < dists[b]; });
      int top_n = std::max(1, (int)std::ceil(ng * 0.1));
      // choose random among top_n
      RNGScope scope;
      int pick_idx = ord[std::floor(R::runif(0, top_n))];
      int chosen_k = good_idx[pick_idx];
      NumericVector out = NumericVector::create(grid_lon[chosen_k], grid_lat[chosen_k]);
      return out;
    }
    expand += expand_step;
  }
  // nothing found: return original position
  return NumericVector::create(start_lon, start_lat);
}
