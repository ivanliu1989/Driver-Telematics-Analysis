#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int updateNextAvailableMinute(int start_minute, int work_duration){
    int next_available_time = 0;
    int S = 0;
    int U = 0;
    int full_days = work_duration / (60*24);
    S = full_days * (10*60);
    U = full_days * (14*60);
    int remainder = start_minute + full_days * (60*24);
    for (int i = remainder; i < (start_minute+work_duration); ++i) {
        bool isSanctionedTime = ((i - 540) % (60*24)) < 600;
        if (isSanctionedTime) S += 1;
        else U += 1;
    }
    int end_minute = start_minute + work_duration;
    
    if(U == 0) {
        if(((end_minute - 540) % (60*24)) < 600) {
            next_available_time = end_minute;
        } else {
            bool isSanctionedTime = ((end_minute - 540) % (60*24)) < 600;
            bool isSanctionedTime_1 = ((end_minute - 539) % (60*24)) < 600;
            int next_min = 0;
            if(isSanctionedTime && isSanctionedTime_1) next_min = end_minute + 1;
            else {
                int num_days = end_minute / (60*24);
                int am_or_pm = (end_minute % (60*24))/ 540;
                next_min = 540 + (num_days + am_or_pm / 2) * (60*24);
            }
            next_available_time = next_min;
        }
    } else {
        if (U == 0) {
            bool isSanctionedTime = ((end_minute - 540) % (60*24)) < 600;
            if (isSanctionedTime) next_available_time = end_minute;
            else{
                bool isSanctionedTime = ((end_minute - 540) % (60*24)) < 600;
                bool isSanctionedTime_1 = ((end_minute - 539) % (60*24)) < 600;
                int next_min = 0;
                if(isSanctionedTime && isSanctionedTime_1) next_min = end_minute + 1;
                else {
                    int num_days = end_minute / (60*24);
                    int am_or_pm = (end_minute % (60*24))/ 540;
                    next_min = 540 + (num_days + am_or_pm / 2) * (60*24);
                }
                next_available_time = next_min;
            }
        }
        int num_days_since_jan1 = end_minute / (60 * 24);
        int rest_time = U;
        int rest_time_in_working_days = rest_time / 600;
        int rest_time_remaining_minutes = rest_time % 600;
        int local_start = end_minute % (60 * 24);
        if (local_start < 540) local_start = 540;
        else if (local_start > 1140) {
            num_days_since_jan1 += 1;
            local_start = 540;
        }
        if (local_start + rest_time_remaining_minutes >= 1140) {
            num_days_since_jan1 += 1;
            rest_time_remaining_minutes -= (1140 - local_start);
            local_start = 540;
        }
        int total_days = num_days_since_jan1 + rest_time_in_working_days;
        
        next_available_time = total_days * (60*24) + local_start + rest_time_remaining_minutes;
    }
    return next_available_time;
}