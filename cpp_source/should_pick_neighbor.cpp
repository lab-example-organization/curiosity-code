#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::export]]
bool cpp_should_pick_neighbor(int index, NumericVector total_chances,
                              int selection_context, int current_chance, 
                              NumericVector sorted_selections, NumericVector selection_filter, 
                              int preferred_bird, double lower, double upper) {

    float lower_bound, upper_bound;
    bool is_desperate, is_neighbor_better = false;
    int neighbor_selection = sorted_selections[preferred_bird + index];

    lower_bound = std::round(total_chances[selection_context] * lower);
    upper_bound = std::round(total_chances[selection_context] * upper);

    is_desperate = lower_bound <= current_chance && upper_bound > current_chance;

    for(int i = 0; i < selection_filter.size(); i++) {
        if(neighbor_selection == selection_filter[i]) {
            is_neighbor_better = true;
            break;
        }
    }

    return is_desperate && is_neighbor_better;
}