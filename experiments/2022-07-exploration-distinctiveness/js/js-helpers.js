var practice_info;
var trial_info;
var experiment_info;
var display_info;
var condition_id;
var n_categories;
var participant_id;
var setup_expt;
var instruction_category;
var stimulus_crp_trial;
var stimulus_cr1_trial;
var stimulus_cr2_trial;
var stimulus_cat_trial;
var category_id;
var category_name;
var stimulus_vals;
var total_trials0;
var total_trials1;
var total_trials2;


if (window.location.search.indexOf('PROLIFIC_PID') > -1) {
    var participant_id = getQueryVariable('PROLIFIC_PID');
}
// If no ID is present, generate one using random numbers - this is useful for testing
else {
    var participant_id = Math.floor(Math.random() * 1000);
}
// STUDY ID
if (window.location.search.indexOf('STUDY_ID') > -1) {
    var studyID = getQueryVariable('STUDY_ID');
}


// get subject ID
function getQueryVariable(variable) {
    var query = window.location.search.substring(1);
    var vars = query.split("&");
    for (var i = 0; i < vars.length; i++) {
        var pair = vars[i].split("=");
        if (pair[0] == variable) { return pair[1]; }
    }
    return (false);
}


// Standard Normal variate using Box-Muller transform.
// source: https://stackoverflow.com/questions/25582882/javascript-math-random-normal-distribution-gaussian-bell-curve
function randn_bm() {
    var u = 0, v = 0;
    while (u === 0) u = Math.random(); //Converting [0,1) to (0,1)
    while (v === 0) v = Math.random();
    return Math.sqrt(-2.0 * Math.log(u)) * Math.cos(2.0 * Math.PI * v);
}


// only accept unique set of samples
// same numbers should not appear twice on a given bandit
function random_normal_samples_unique(m, sd, n_numbers) {
    var all_same = 0;
    var arr_test = [0, 1];
    var numbers_sampled;
    while (all_same < n_numbers) {
        numbers_sampled = Array(n_numbers).fill().map(
            () => Math.round(randn_bm() * sd + m)
        );
        all_same = [...new Set(numbers_sampled)].length;
    }
    return (numbers_sampled)
}


function setup_experiment() {
    document.getElementById("t_start").innerHTML = Date.now();
    // experiment information
    var experiment_info = {
        "var_mem_test": [true, false],
        "var_horizon": [1, 6],
        "var_distinct": ["massed", "interleaved"],
        "var_location_test": Array([0, 1], [1, 0]),
        "var_mean_shift": [-20, -10, -4, 4, 10, 20],
        "n_trials_practice": 2, // 1. memory test + horizon 12 + massed; 2. no memory test + horizon 1 + interleaved
        "n_trials_per_condition": 2,
        "bandit_means": [40, 60],
        "bandit_sd": [10],
        "n_vals": 12,
        "n_forced_choice": 6,
        "sequences_forced_choices": {
            "massed": [[0, 0, 0, 1, 1, 1], [1, 1, 1, 0, 0, 0]],
            "interleaved": [[0, 1, 0, 1, 0, 1], [1, 0, 1, 0, 1, 0]]
        }
    }
    experiment_info["n_trials"] = ( // no memory test
        experiment_info["n_trials_per_condition"] * experiment_info["var_distinct"].length *
        experiment_info["var_horizon"].length * experiment_info["var_mean_shift"].length
    ) + (
            experiment_info["n_trials_per_condition"] * experiment_info["var_distinct"].length *
            experiment_info["var_horizon"].length * experiment_info["var_location_test"].length *
            experiment_info["var_mean_shift"].length
        );
    experiment_info["n_trials"] = (
        experiment_info["n_trials_per_condition"] *
        experiment_info["var_mem_test"].length *
        experiment_info["var_horizon"].length *
        experiment_info["var_distinct"].length
    )
    //experiment_info["n_trials"] = 2 * 2 * 2 * 2; // [no mem test, left first, right first] * [horizon 1, horizon 6] * [massed, interleaved] * [mean shifts (1 only for testing, otherwise 6)]

    // display info
    var display_info = {
        iti: 500,
        presentation: 1000,
    }

    // practice info
    var practice_info = {
        "memory_test": [],
        "location_test": [],
        "horizon": [],
        "distinctiveness": [],
        "vals_bandit_0": [],
        "vals_bandit_1": [],
        "sequence_forced_choices": [],
        "mean_bandit_0": [],
        "mean_bandit_1": [],
    }

    // trial info
    var trial_info = {
        "memory_test_prep": [],
        "horizon_prep": [],
        "distinctiveness_prep": [],
        "sequence_forced_choices_prep": [],
        "location_test_prep": [],
        "mean_shift_prep": [],
        "memory_test": [],
        "horizon": [],
        "distinctiveness": [],
        "sequence_forced_choices": [],
        "location_test": [],
        "vals_bandit_0": [],
        "vals_bandit_1": [],
        "mean_bandit_0": [],
        "mean_bandit_1": [],
    };

    // loop over three within-participant variables
    // generate n_trials_per_condition replications of the same combination
    var idx = 0;
    for (const mem of experiment_info["var_mem_test"].entries()) {
        for (const hor of experiment_info["var_horizon"].entries()) {
            for (const dis of experiment_info["var_distinct"].entries()) {
                //for (const ms of experiment_info["var_mean_shift"].entries()) {
                var var_num = Math.floor(6 * Math.random());
                var ms = [0, experiment_info["var_mean_shift"][var_num]];
                for (let rep = 0; rep < experiment_info["n_trials_per_condition"]; rep++) {
                    if (mem[1] == false) {
                        trial_info["memory_test_prep"][idx] = mem[1];
                        trial_info["horizon_prep"][idx] = hor[1];
                        trial_info["distinctiveness_prep"][idx] = dis[1];
                        var rand_position = Math.floor(Math.random() * 2)
                        trial_info["sequence_forced_choices_prep"][idx] = experiment_info["sequences_forced_choices"][dis[1]][rand_position];
                        trial_info["location_test_prep"][idx] = "none"; // no test location because no memory test
                        trial_info["mean_shift_prep"][idx] = ms[1];
                        idx += 1;
                    } else if (mem[1] == true) {
                        //for (const loc of experiment_info["var_location_test"].entries()) {
                        var rand_presentation = Math.floor(Math.random() * 2)
                        var rand_test = Math.floor(Math.random() * 2)
                        trial_info["memory_test_prep"][idx] = mem[1];
                        trial_info["horizon_prep"][idx] = hor[1];
                        trial_info["distinctiveness_prep"][idx] = dis[1];
                        trial_info["sequence_forced_choices_prep"][idx] = experiment_info["sequences_forced_choices"][dis[1]][rand_presentation];
                        trial_info["location_test_prep"][idx] = experiment_info["var_location_test"][rand_test];//loc[1];
                        trial_info["mean_shift_prep"][idx] = ms[1];
                        idx += 1;
                        //}
                    }
                }
                //}
            }
        }
    };

    // shuffle all the generated trials randomly
    shuffle_trials = Array(experiment_info["n_trials"]).fill().map((element, index) => index);
    shuffle_trials = append_randomized_arrays(shuffle_trials, 1);
    for (let idx = 0; idx < experiment_info["n_trials"]; idx++) {
        if (shuffle_trials[idx] % 2 == 0) {
            assign_mean = 0
        } else { assign_mean = 1 }
        var mean_0 = experiment_info["bandit_means"][assign_mean];
        var mean_1 = experiment_info["bandit_means"][assign_mean] + trial_info["mean_shift_prep"][shuffle_trials[idx]];
        var rand_mean_location = Math.random() >= .5 | 0;
        if (rand_mean_location == true) {
            bd_means = [mean_0, mean_1];
        } else {
            bd_means = [mean_1, mean_0];
        }
        trial_info["sequence_forced_choices"][idx] = trial_info["sequence_forced_choices_prep"][shuffle_trials[idx]];
        trial_info["memory_test"][idx] = trial_info["memory_test_prep"][shuffle_trials[idx]];
        trial_info["location_test"][idx] = trial_info["location_test_prep"][shuffle_trials[idx]];
        trial_info["horizon"][idx] = trial_info["horizon_prep"][shuffle_trials[idx]];
        trial_info["distinctiveness"][idx] = trial_info["distinctiveness_prep"][shuffle_trials[idx]];
        trial_info["vals_bandit_0"][idx] = random_normal_samples_unique(
            bd_means[0], experiment_info["bandit_sd"], experiment_info["n_vals"]
        );
        trial_info["vals_bandit_1"][idx] = random_normal_samples_unique(
            bd_means[1], experiment_info["bandit_sd"], experiment_info["n_vals"]
        );
        trial_info["mean_bandit_0"][idx] = bd_means[0];
        trial_info["mean_bandit_1"][idx] = bd_means[1];
    }

    // fill practice such that all values of variables seen once
    // first practice trial
    practice_info["memory_test"][0] = true;
    practice_info["location_test"][0] = [0, 1];
    practice_info["horizon"][0] = experiment_info["var_horizon"][1];
    practice_info["distinctiveness"][0] = "massed";
    practice_info["vals_bandit_0"][0] = random_normal_samples_unique(
        experiment_info["bandit_means"][0], experiment_info["bandit_sd"], experiment_info["n_vals"]
    );
    practice_info["vals_bandit_1"][0] = random_normal_samples_unique(
        experiment_info["bandit_means"][1], experiment_info["bandit_sd"], experiment_info["n_vals"]
    );
    practice_info["sequence_forced_choices"][0] = experiment_info["sequences_forced_choices"]["massed"][0];
    practice_info["mean_bandit_0"][0] = experiment_info["bandit_means"][0];
    practice_info["mean_bandit_1"][0] = experiment_info["bandit_means"][1];

    // second practice trial
    practice_info["memory_test"][1] = false;
    practice_info["location_test"][1] = "none";
    practice_info["horizon"][1] = experiment_info["var_horizon"][0];
    practice_info["distinctiveness"][1] = "interleaved";
    practice_info["vals_bandit_0"][1] = random_normal_samples_unique(
        experiment_info["bandit_means"][0], experiment_info["bandit_sd"], experiment_info["n_vals"]
    );
    practice_info["vals_bandit_1"][1] = random_normal_samples_unique(
        experiment_info["bandit_means"][1], experiment_info["bandit_sd"], experiment_info["n_vals"]
    );
    practice_info["sequence_forced_choices"][1] = experiment_info["sequences_forced_choices"]["interleaved"][1];
    practice_info["mean_bandit_0"][1] = experiment_info["bandit_means"][0];
    practice_info["mean_bandit_1"][1] = experiment_info["bandit_means"][1];

    var obj_setup_expt;
    obj_setup_expt = {
        experiment_info: experiment_info,
        display_info: display_info,
        trial_info: trial_info,
        practice_info: practice_info
    }
    return obj_setup_expt
}


function setup_and_proceed() {
    setup_expt = setup_experiment()
    experiment_info = setup_expt["experiment_info"]
    display_info = setup_expt["display_info"]
    practice_info = setup_expt["practice_info"]
    trial_info = setup_expt["trial_info"]
    clickStart('page0', 'page1')
}


function append_randomized_arrays(set, n) {
    var sets_randomized = [];
    for (let i = 0; i < n; i++) {
        var set_permuted = permute(set)
        sets_randomized = sets_randomized.concat(set_permuted);
    }
    return sets_randomized
}

//permute a list
function permute(o) {
    for (var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o;
};


//function to hide one html div and show another
function clickStart(hide, show) {
    document.getElementById(hide).style.display = 'none';
    document.getElementById(show).style.display = 'block';
    window.scrollTo(0, 0);
}

//https://stackoverflow.com/questions/951021/what-is-the-javascript-version-of-sleep
function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

function progress_in_experiment() {
    part = parseInt(document.getElementById("part_experiment").innerHTML)
    if (part == 0) { // practice
        i = parseInt(document.getElementById("trial_nr_practice").innerHTML)
    }
    if (part == 1) { // experimental trials
        i = parseInt(document.getElementById("trial_nr_main").innerHTML)
    }
    if (part == 0) {
        current_info = practice_info;
    } else if (part == 1) {
        current_info = trial_info;
    }
    return [part, i, current_info]
}


async function display_forced_choices(old, part_experiment = null) {
    // old refers to page to be switched away from
    // reset the reward counter
    document.getElementById("cumulative_value").innerHTML = 0;
    document.getElementById("cumulative_value_str").innerHTML = "Collected Amount = 0";

    if (part_experiment != null) {
        document.getElementById("part_experiment").innerHTML = part_experiment;
    }
    var [part, i, current_info] = progress_in_experiment();

    clickStart(old, 'page6')

    var item_id = 0;
    display_option = cue_location(i, item_id, part)
    display_option.onclick = function () { next_value_forced(i, item_id, part) }
}


function cue_location(i, item_id, part) {
    if (part == 0) { // practice
        current_option = practice_info["sequence_forced_choices"][i][item_id]
    } else if (part == 1) {
        current_option = trial_info["sequence_forced_choices"][i][item_id]
    }
    location_display = "value_displayed_" + current_option;
    display_option = document.getElementById(location_display);
    display_option.style.background = "#26dabcde";
    display_option.innerHTML = "?"
    return (display_option)
}


async function next_value_forced(i, item_id, part) {
    if (part == 0) {
        current_info = practice_info;
    } else if (part == 1) {
        current_info = trial_info;
    }
    // read out current choice value
    current_option = current_info["sequence_forced_choices"][i][item_id]
    value_display = "vals_bandit_" + current_option
    location_display = "value_displayed_" + current_option
    display_option = document.getElementById(location_display);
    // remove cued background color, display value, and remove it again
    display_option.style.background = "white";
    display_option.innerHTML = current_info[value_display][i][item_id];
    await sleep(display_info["presentation"]);
    display_option.innerHTML = "";
    // increase choice counter by one
    item_id += 1;
    // display another item or go to the memory test
    if (item_id < experiment_info["n_forced_choice"]) {//4) {//
        display_option = cue_location(i, item_id, part);
        display_option.onclick = function () {
            next_value_forced(i, item_id, part)
        }
    } else if (item_id == experiment_info["n_forced_choice"]) {//{//
        // increase trial nr by 1
        document.getElementById("time_var").innerHTML = Date.now();
        if (current_info["memory_test"][i] == true) {
            cue_memory_responses1(current_info["location_test"][i][0]);
        } else if (current_info["memory_test"][i] == false) {
            display_free_choices("page6", experiment_info["n_vals"] / 2) // second arg should be item_id
        }
    }
}

function cue_memory_responses1(i) {
    document.getElementById("mem_response_a").value = "";
    document.getElementById("response_displayed_a_0").innerHTML = "";
    document.getElementById("response_displayed_a_1").innerHTML = "";
    clickStart("page6", "page7a")
    location_display = "response_displayed_a_" + i;
    display_option = document.getElementById(location_display);
    display_option.innerHTML = "?"
    display_option.style.background = "#26dabcde";
    document.getElementById("time_var").innerHTML = Date.now();
}

function cue_memory_responses2(i) {
    document.getElementById("mem_response_b").value = "";
    document.getElementById("response_displayed_b_0").innerHTML = "";
    document.getElementById("response_displayed_b_1").innerHTML = "";
    clickStart("page7a", "page7b")
    location_display = "response_displayed_b_" + i;
    display_option = document.getElementById(location_display);
    display_option.innerHTML = "?"
    display_option.style.background = "#26dabcde";
    document.getElementById("time_var").innerHTML = Date.now();
}

function link_mem_responses() {
    var rt_mem = Date.now() - document.getElementById("time_var").innerHTML;
    document.getElementById("rt_mem").innerHTML = rt_mem;
    process_memory_responses("a");
    document.getElementById("mem_response_a").value = "";
    var [_, i, current_info] = progress_in_experiment();
    var loc_test2 = current_info["location_test"][i][1];
    cue_memory_responses2(loc_test2);
}

function clean_and_proceed() {
    var rt_mem = Date.now() - document.getElementById("time_var").innerHTML;
    document.getElementById("rt_mem").innerHTML = rt_mem;
    process_memory_responses("b");
    // input field is wiped again
    document.getElementById("mem_response_b").value = "";
    document.getElementById("cumulative_value").innerHTML = 0;
    display_free_choices('page7b', experiment_info["n_vals"] / 2)
}

// display cumulative value on free-choice items


function process_memory_responses(step) {
    var [part, i, current_info] = progress_in_experiment();
    var loc_idx;
    if (step == "a") {
        loc_idx = 0;
    } else if (step == "b") {
        loc_idx = 1;
    }
    loc_test = current_info["location_test"][i][loc_idx]; // left or right tested?
    loc_pres = current_info["sequence_forced_choices"][i][0]; // first item in sequnce left or right?


    var mem_response = document.getElementById("mem_response_" + step).value;
    var mem_response_trim = mem_response.trim();
    var mem_response_split = mem_response_trim.split(",");
    var mem_response_split_unique = [...new Set(mem_response_split)];
    var side_tested = current_info["location_test"][i][loc_idx]
    var vals_shown = "vals_bandit_" + side_tested;
    var shown_list_all = current_info[vals_shown][i].slice(0, experiment_info["n_forced_choice"]);
    var shown_list_mask = current_info["sequence_forced_choices"][i].map(item => item == side_tested)
    var shown_list = shown_list_all.filter((item, i) => shown_list_mask[i])

    var count_accuracy = 0;
    var count_redundant = 0;
    var mem_response_int_unique = Array();
    for (const response of mem_response_split_unique) {
        if (response !== "") {
            if (shown_list.includes(parseInt(response))) {
                count_accuracy += 1;
            } else {
                count_redundant += 1;
            }
            mem_response_int_unique.push(parseInt(response))
        }

    }
    document.getElementById("memory_cum").innerHTML = parseInt(document.getElementById("memory_cum").innerHTML) + count_accuracy;
    log_memory_responses(shown_list, count_accuracy, count_redundant, mem_response_int_unique, loc_idx, loc_test, loc_pres)
}


function log_memory_responses(shown_list, count_accuracy, count_redundant, responses_unique, loc_idx, loc_test, loc_pres) {
    var [part, i, current_info] = progress_in_experiment();
    var rt_mem = document.getElementById("rt_mem").innerHTML;
    var data_store = {
        participant_id: participant_id,
        session: part,
        trial_id: i,
        presentation: current_info["distinctiveness"][i],
        horizon: current_info["horizon"][i],
        is_memtest: current_info["memory_test"][i],
        test_cue_nr: loc_idx,
        test_cue_pos: loc_test,
        first_item_pres: current_info["sequence_forced_choices"][i][0],
        first_item_cue: current_info["location_test"][i][0],
        true_mean: current_info["mean_bandit_" + loc_idx][i],
        items: shown_list,
        responses_unique: responses_unique,
        n_correct: count_accuracy,
        n_redundant: count_redundant,
        rt: rt_mem
    }
    saveData(JSON.stringify(data_store), "memory");
}


async function display_free_choices(old, item_id) {

    var [part, i, current_info] = progress_in_experiment();
    var display_option_a;
    var display_option_b;

    clickStart(old, "page6");

    document.getElementById("cumulative_value_str").style.display = "block";
    if (item_id <= parseInt(current_info["horizon"][i]) + parseInt((experiment_info["n_forced_choice"] - 1))) {
        [display_option_a, display_option_b] = format_both_options("question");
        document.getElementById("time_var").innerHTML = Date.now()
        display_option_a.onclick = function () { next_value_free(i, item_id, current_info, 0) }
        display_option_b.onclick = function () { next_value_free(i, item_id, current_info, 1) }
        var n_remaining = ((current_info["horizon"][i] + (experiment_info["n_forced_choice"] - 1)) - item_id + 1);

        document.getElementById("n_remaining_choices").style.display = "block"
        document.getElementById("n_remaining_choices").innerHTML = "Nr. remaining choices = " + n_remaining
        if (n_remaining == 1) {
            document.getElementById("n_remaining_choices").style.color = "red"
        } else if (n_remaining <= experiment_info["var_horizon"][1] / 2) {
            document.getElementById("n_remaining_choices").style.color = "orange"
        } else {
            document.getElementById("n_remaining_choices").style.color = "green"
        }
    } else {
        document.getElementById("n_remaining_choices").style.color = "black"
        document.getElementById("n_remaining_choices").style.display = "none"
        document.getElementById("cumulative_value_str").style.display = "none";
        update_trial_counter(part, i);
        if (part == 0 & i == (experiment_info["n_trials_practice"] - 1)) {
            // practice is over
            format_both_options("reset")
            clickStart("page6", "page9")
        } else if (part == 1 & i == (experiment_info["n_trials"] - 1)) {
            // experiment is over
            time_taken();
            calculate_bonus();
            clickStart("page6", "page10")
        } else {
            // next trial
            format_both_options("reset")
            clickStart("page6", "page8")
        }
    }
}

function time_taken() {
    var total_time = Date.now() - document.getElementById("t_start").innerHTML;
    var time_store = {
        participant_id: participant_id,
        total_time: total_time,
        total_time_min: Math.ceil((total_time / 1000) / 60)
    }
    saveTime(JSON.stringify(time_store));
}


// todos
// counter of total received rewards from free choice trials
// counter of maximally possible rewards
// function financial_reward_in_study dividing the two
// have a counter of the number of correctly recalled items
// sum of total number of items possibly recallable
// again divide the two in the financial_reward_in_study function`


async function next_value_free(i, item_id, current_info, pos) {
    // read out current choice value
    var clickflag = parseInt(document.getElementById("block_responding").innerHTML);
    if (clickflag == 0) {
        document.getElementById("block_responding").innerHTML = 1;
        var rt_choice = Date.now() - document.getElementById("time_var").innerHTML;
        document.getElementById("rt_choice").innerHTML = rt_choice;
        value_display = "vals_bandit_" + pos
        location_display = "value_displayed_" + pos
        display_option = document.getElementById(location_display);
        // remove cued background color, display value, and remove it again
        display_option.style.background = "white";
        var chosen_value = current_info[value_display][i][item_id];
        display_option.innerHTML = chosen_value;
        var max_possible = Math.max(current_info["vals_bandit_0"][i][item_id], current_info["vals_bandit_1"][i][item_id]);
        document.getElementById("cumulative_value").innerHTML = parseInt(document.getElementById("cumulative_value").innerHTML) + parseInt(chosen_value);
        document.getElementById("cumulative_value_str").innerHTML = "Collected Amount = " + parseInt(document.getElementById("cumulative_value").innerHTML);
        document.getElementById("reward_cum").innerHTML = parseInt(document.getElementById("reward_cum").innerHTML) + parseInt(chosen_value);
        document.getElementById("reward_possible_cum").innerHTML = parseInt(document.getElementById("reward_possible_cum").innerHTML) + max_possible;

        await sleep(display_info["presentation"]);
        display_option.style.background = "#26dabcde";
        display_option.innerHTML = "?";
        log_choice(item_id, pos)
        item_id += 1;
        document.getElementById("block_responding").innerHTML = 0;
        display_free_choices("page6", item_id)
    }
}


function format_both_options(direction) {
    if (direction == "reset") {
        display_option_a = document.getElementById("value_displayed_0");
        display_option_b = document.getElementById("value_displayed_1");
        display_option_a.innerHTML = ""
        display_option_b.innerHTML = ""
        display_option_a.style.background = "white";
        display_option_b.style.background = "white";
    } else if (direction == "question") {
        display_option_a = document.getElementById("value_displayed_0");
        display_option_b = document.getElementById("value_displayed_1");
        display_option_a.innerHTML = "?"
        display_option_b.innerHTML = "?"
        display_option_a.style.background = "#26dabcde";
        display_option_b.style.background = "#26dabcde";
    }
    return [display_option_a, display_option_b]
}


function log_choice(item_id, choice) {
    var [part, i, current_info] = progress_in_experiment();
    var rt_choice = document.getElementById("rt_choice").innerHTML;
    var data_store = {
        participant_id: participant_id,
        session: part,
        trial_id: i,
        item_id: item_id,
        presentation: current_info["distinctiveness"][i],
        horizon: current_info["horizon"][i],
        is_memtest: current_info["memory_test"][i],
        first_item_pres: current_info["sequence_forced_choices"][i][0],
        first_item_cue: current_info["location_test"][i][0],
        mean_left: current_info["mean_bandit_0"][i],
        mean_right: current_info["mean_bandit_1"][i],
        val_l: current_info["vals_bandit_0"][i][item_id],
        val_r: current_info["vals_bandit_1"][i][item_id],
        choice: choice,
        rt: rt_choice
    }
    saveData(JSON.stringify(data_store), "choice");
}



function update_trial_counter(part, i) {
    var i_new = i + 1
    switch (part) {
        case 0:
            document.getElementById("trial_nr_practice").innerHTML = i_new
            break;
        case 1:
            document.getElementById("trial_nr_main").innerHTML = i_new
            break;
    }
}

function saveData(filedata, task) {
    var filename = "./data/" + task + "-participant-" + participant_id + ".json";
    $.post("save_data.php", { postresult: filedata + "\n", postfile: filename })
}

function download(content, fileName, contentType) {
    var a = document.createElement("a");
    var file = new Blob([content], { type: contentType });
    a.href = URL.createObjectURL(file);
    a.download = fileName;
    a.click();
}


// color text
function color(id, col) {
    document.getElementById(id).style.color = col;
}


function colorWrongAnswer(question, col) {
    const rbs = document.querySelectorAll('input[name="' + question + '\"]');
    for (const rb of rbs) {
        if (rb.checked) {
            color(question + rb.id, col)
            break;
        }
    }
}


function checkOnPage(page) {
    if (document.getElementById(page).style.display == 'block') { return true }
    else { return false }
}


//changes inner HTML of div with ID=x to y
function change(x, y) {
    document.getElementById(x).innerHTML = y;
}


function changeColor(element, color) {
    document.getElementById(element).style.color = color;
}


var flag = 0;
var instcounter = 0;
function instructioncheck(pg, pg_prev, pg_succeed) {
    var ch1 = 0;
    var ch2 = 0;
    var ch3 = 0;
    var ch4 = 0;
    var ch5 = 0;
    //check if correct answers are provided
    if (document.getElementById('icheck1').checked) { var ch1 = 1; color('q1icheck1', 'green') }
    else { colorWrongAnswer("q1", 'red') }
    if (document.getElementById('icheck2').checked) { var ch2 = 1; color('q2icheck2', 'green') }
    else { colorWrongAnswer("q2", 'red') }
    if (document.getElementById('icheck3').checked) { var ch3 = 1; color('q3icheck3', 'green') }
    else { colorWrongAnswer("q3", 'red') }
    if (document.getElementById('icheck4').checked) { var ch4 = 1; color('q4icheck4', 'green') }
    else { colorWrongAnswer("q4", 'red') }
    if (document.getElementById('icheck5').checked) { var ch5 = 1; color('q5icheck5', 'green') }
    else { colorWrongAnswer("q5", 'red') }
    var checksum = ch1 + ch2 + ch3 + ch4 + ch5;
    var criterion = 5;

    // indicate correct answers
    ++flag;
    clickStart(pg, pg);
    change("check", "Continue")

    // page transition 
    if ((checksum === criterion) && (flag == 2)) {
        //if correct, continue 
        //begintrial();
        clickStart(pg, pg_succeed);
        // alert
        alert('Great, you have answered all of the questions correctly. The study will now start.');
    }
    else {
        if (flag == 2) {
            instcounter++;
            colorWrongAnswer("q1", '#333333')
            colorWrongAnswer("q2", '#333333')
            colorWrongAnswer("q3", '#333333')
            colorWrongAnswer("q4", '#333333')
            colorWrongAnswer("q5", '#333333')
            //if one or more answers are wrong, raise alert box
            alert('You have answered some of the questions wrong. Please try again.');
            // go back to instructions
            clickStart(pg, pg_prev);
            flag = 0;

        }
    }

}


function saveBonus(filedata) {
    var filename = "./data/bonus.json";
    $.post("save_data.php", { postresult: filedata + "\n", postfile: filename })
}

function saveTime(filedata) {
    var filename = "./data/total-time.json";
    $.post("save_data.php", { postresult: filedata + "\n", postfile: filename })
}


function calculate_bonus() {

    // bonus continuous reproduction
    const bonus_mem_max = 3.60
    var n_trials = parseInt(2 * trial_info["memory_test"].length) / 3 + 1;
    var n_memory_items = n_trials * experiment_info["n_forced_choice"];
    var n_memory_correct = parseFloat(document.getElementById("memory_cum").innerHTML)
    var prop_correct_memory = n_memory_correct / n_memory_items
    var bonus_mem = Math.round(prop_correct_memory * bonus_mem_max * 100) / 100;

    var bonus_choice_max = 3.60
    var received = document.getElementById("reward_cum").innerHTML;
    var possible = document.getElementById("reward_possible_cum").innerHTML;
    var prop_possible = received / possible;
    var bonus_choice = Math.round(prop_possible * bonus_choice_max * 100) / 100;


    var bonus_total = Math.round((bonus_mem + bonus_choice) * 100) / 100;


    var bonus_store = {
        participant_id: participant_id,
        bonus_mem: bonus_mem,
        bonus_choice: bonus_choice,
        bonus_total: bonus_total
    }

    saveBonus(JSON.stringify(bonus_store));

    (() => {
        document.getElementById("total_bonus").innerHTML = bonus_total;
        document.getElementById("mem_bonus").innerHTML = bonus_mem;
        document.getElementById("choice_bonus").innerHTML = bonus_choice;
    })();

}

function redirect_to_prolific() {
    window.location.href = "https://app.prolific.co/submissions/complete?cc=240D34C0";
}
