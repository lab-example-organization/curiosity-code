#include <Rcpp.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
bool cpp_update_selexn_data(NumericVector one_pop_singers, List moran, 
                            NumericVector suitor_choices,
                            int preferred_bird, int selector_bird,
                            NumericVector curiosity_value,
                            int selector_population,
                            int selection_context, 
                            NumericMatrix sylreps_choices,
                            NumericVector sylrep_selector, int selection_count,
                            bool giving_up) {
    int singer_population;
    int[2] selected_pair;
    vector<int>[2] sylrep_pairs;
    float[2] curiosities;
    int pool_row;
    
    if(!giving_up) {
        singer_population = std::ceil(float(preferred_bird)/one_pop_singers[selection_context]);
        selected_pair = {suitor_choices[preferred_bird], selector_bird};
        for(int i = 0; i < 2; i++) {
            vector<int> temp;
            for(int j = 0; j < sylreps_choices(preferred_bird, _).size(); j++) {
                temp.push_back(sylreps_choices[preferred_bird, j]);
            }
            sylrep_pairs[i] = temp;
        }
        curiosities = {curiosity_value[selected_pair[0], singer_population],
                       curiosity_value[selected_pair[1], selector_population]};
    } else {
        singer_population = selector_population;
        selected_pair = {suitor_choices[preferred_bird], selector_bird};
        for(int i = 0; i < 2; i++) {
            vector<int> temp;
            for(int j = 0; j < sylreps_choices(preferred_bird, _, singer.population).size(); j++) {
                temp.push_back(sylreps_choices[preferred_bird, j]);
            }
            sylrep_pairs[i] = temp;
        }
        curiosities = {curiosity_value[selected_pair[0], singer_population],
                       curiosity_value[selected_pair[1], selector_population]};
    }
    for(int i = 0; i < selection_context; i++) {
        
    }
}
    
 /*
update_selexn_data <- function(main_parameters, moran, suitor_choices, preferred_bird, selector_bird,
                               curiosity_value, selector_population, selection_context, 
                               sylreps_choices, sylrep_selector, selection_count, giving_up = FALSE) {
  if(!(giving_up)) {
    singer_population <- ceiling(preferred_bird/main_parameters$one_pop_singers[selection_context])
    
    selected_pair <- c(suitor_choices[preferred_bird], # Bird being selected
                       selector_bird)          # Bird doing the selecting
    sylrep_pairs <- rbind(sylreps_choices[preferred_bird,],
                          sylrep_selector)
    curiosities <- c(curiosity_value[selected_pair[1],singer_population],
                     curiosity_value[selected_pair[2],selector_population])
  } else {
    singer_population <- selector_population 
    
    selected_pair <- c(suitor_choices[preferred_bird], # Bird being selected
                       selector_bird)          # Bird doing the selecting
    sylrep_pairs <- rbind(sylreps_choices[preferred_bird,,singer_population],
                          sylrep_selector)
    curiosities <- c(curiosity_value[selected_pair[1],singer_population],
                     curiosity_value[selected_pair[2],selector_population])
  } 
  
  for(bird in 1:selection_context) {
    pool.row <- (5^(2-selection_context)) * bird
    moran$pairing.pool[pool.row, 1, selector_population] <- selected_pair[bird]
    moran$learning.pool[pool.row, , selector_population] <- sylrep_pairs[bird,]
    moran$pairing.pool[pool.row, 2, selector_population] <- curiosities[bird]
  }

  moran$pairing.pool[(4 - selection_context), 3, selector_population] <- selection_count
  return(moran)
}


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
 */