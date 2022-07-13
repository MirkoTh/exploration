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


function setup_experiment() {
    // experiment information
    var experiment_info = {
        "var_mem_test": [true, false],
        "var_horizon": [1, 12],
        "var_distinct": ["massed", "interleaved"],
        "var_location_test": [0, 1],
        "n_trials_practice": 2, // 1. memory test + horizon 12 + massed; 2. no memory test + horizon 1 + interleaved
        "n_trials_per_condition": 3,
        "bandit_means": [10, 20],
        "bandit_sd": [20],
        "n_vals": 24,
        "n_forced_choice": 12,
        "sequences_forced_choices": {
            "massed": [0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1],
            "interleaved": [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
        }
    }
    experiment_info["n_trials"] = (
        experiment_info["n_trials_per_condition"] * experiment_info["var_distinct"].length *
        experiment_info["var_horizon"].length * experiment_info["var_mem_test"].length
    )

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
    }

    // trial info
    var trial_info = {
        "memory_test_prep": [],
        "horizon_prep": [],
        "distinctiveness_prep": [],
        "sequence_forced_choices_prep": [],
        "location_test_prep": [],
        "memory_test": [],
        "horizon": [],
        "distinctiveness": [],
        "sequence_forced_choices": [],
        "location_test": [],
        "vals_bandit_0": [],
        "vals_bandit_1": []
    };

    // loop over three within-participant variables
    // generate n_trials_per_condition replications of the same combination
    var idx = 0;
    for (const mem of experiment_info["var_mem_test"].entries()) {
        for (const hor of experiment_info["var_horizon"].entries()) {
            for (const dis of experiment_info["var_distinct"].entries()) {
                for (const loc of experiment_info["var_location_test"].entries()) {
                    for (let rep = 0; rep < experiment_info["n_trials_per_condition"]; rep++) {
                        trial_info["memory_test_prep"][idx] = mem[1];
                        trial_info["horizon_prep"][idx] = hor[1];
                        trial_info["distinctiveness_prep"][idx] = dis[1];
                        trial_info["sequence_forced_choices_prep"][idx] = experiment_info["sequences_forced_choices"][dis[1]];
                        trial_info["location_test_prep"][idx] = loc[1];
                        idx += 1;
                    }
                }
            }
        }
    };

    // shuffle all the generated trials randomly
    shuffle_trials = Array(experiment_info["n_trials"]).fill().map((element, index) => index);
    shuffle_trials = append_randomized_arrays(shuffle_trials, 1);
    for (let idx = 0; idx < experiment_info["n_trials"]; idx++) {
        trial_info["sequence_forced_choices"][idx] = trial_info["sequence_forced_choices_prep"][shuffle_trials[idx]];
        trial_info["memory_test"][idx] = trial_info["memory_test_prep"][shuffle_trials[idx]];
        trial_info["location_test"][idx] = trial_info["location_test_prep"][shuffle_trials[idx]];
        trial_info["horizon"][idx] = trial_info["horizon_prep"][shuffle_trials[idx]];
        trial_info["distinctiveness"][idx] = trial_info["distinctiveness_prep"][shuffle_trials[idx]];
        trial_info["vals_bandit_0"][idx] = Array(experiment_info["n_vals"]).fill().map(
            () => Math.round(randn_bm() * experiment_info["bandit_sd"] + experiment_info["bandit_means"][0])
        );
        trial_info["vals_bandit_1"][idx] = Array(experiment_info["n_vals"]).fill().map(
            () => Math.round(randn_bm() * experiment_info["bandit_sd"] + experiment_info["bandit_means"][1])
        );
    }

    // fill practice such that all values of variables seen once
    // first practice trial
    practice_info["memory_test"][0] = true;
    practice_info["location_test"][0] = 0;
    practice_info["horizon"][0] = 12;
    practice_info["distinctiveness"][0] = "massed";
    practice_info["vals_bandit_0"][0] = Array(experiment_info["n_vals"]).fill().map(
        () => Math.round(randn_bm() * experiment_info["bandit_sd"] + experiment_info["bandit_means"][0])
    );
    practice_info["vals_bandit_1"][0] = Array(experiment_info["n_vals"]).fill().map(
        () => Math.round(randn_bm() * experiment_info["bandit_sd"] + experiment_info["bandit_means"][1])
    );
    practice_info["sequence_forced_choices"][0] = [0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1];
    // second practice trial
    practice_info["memory_test"][1] = false;
    practice_info["location_test"][1] = 1;
    practice_info["horizon"][1] = 1;
    practice_info["distinctiveness"][1] = "interleaved";
    practice_info["vals_bandit_0"][1] = Array(experiment_info["n_vals"]).fill().map(
        () => Math.round(randn_bm() * experiment_info["bandit_sd"] + experiment_info["bandit_means"][0])
    );
    practice_info["vals_bandit_1"][1] = Array(experiment_info["n_vals"]).fill().map(
        () => Math.round(randn_bm() * experiment_info["bandit_sd"] + experiment_info["bandit_means"][1])
    );
    practice_info["sequence_forced_choices"][1] = [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1];


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
    clickStart('page0', 'page3')
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
    // i refers to the current trial to be displayed
    if (part_experiment != null) {
        document.getElementById("part_experiment").innerHTML = part_experiment;
    }
    var [part, i, current_info] = progress_in_experiment();

    clickStart(old, 'page5')

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
    if (item_id < 4) {//experiment_info["n_forced_choice"] - 1) {//
        display_option = cue_location(i, item_id, part);
        display_option.onclick = function () {
            next_value_forced(i, item_id, part)
        }
    } else {//if (item_id == experiment_info["n_forced_choice"] - 1) {//
        // increase trial nr by 1
        document.getElementById("time_var").innerHTML = Date.now();
        if (current_info["memory_test"][i] == true) {
            cue_memory_responses(current_info["location_test"][i]);
        } else {
            display_free_choices("page5", 12) // second arg should be item_id
        }
    }
}

function cue_memory_responses(i) {
    document.getElementById("mem_response").value = "";
    document.getElementById("response_displayed_0").innerHTML = "";
    document.getElementById("response_displayed_1").innerHTML = "";
    clickStart("page5", "page6")
    location_display = "response_displayed_" + i;
    display_option = document.getElementById(location_display);
    display_option.innerHTML = "?"
    display_option.style.background = "#26dabcde";
}

function clean_and_proceed() {
    // x can be used to save data
    save_memory_responses();
    // input field is wiped again
    document.getElementById("mem_response").value = "";
    display_free_choices('page6', 12)
}

// check that the same number is not sampled twice on a given trial 
function save_memory_responses() {
    var [part, i, current_info] = progress_in_experiment();

    var mem_response = document.getElementById("mem_response").value;
    var mem_response_trim = mem_response.trim();
    var mem_response_split = mem_response_trim.split(",");
    var mem_response_split_unique = [...new Set(mem_response_split)];
    var side_tested = current_info["location_test"][i]
    var vals_shown = "vals_bandit_" + side_tested;
    var shown_list_all = current_info[vals_shown][i].slice(0, experiment_info["n_forced_choice"]);
    var shown_list_mask = current_info["sequence_forced_choices"][i].map(item => item == side_tested)
    var shown_list = shown_list_all.filter((item, i) => shown_list_mask[i])

    var count_accuracy = 0;
    var count_redundant = 0;
    for (const response of mem_response_split_unique) {
        if (shown_list.includes(parseInt(response))) {
            count_accuracy += 1;
        } else {
            count_redundant += 1;
        }
    }
    console.log("nr. correctly recalled items: " + count_accuracy)
    console.log("nr. redundantly recalled items: " + count_redundant)
}


async function next_value_free(i, item_id, current_info, pos) {
    // read out current choice value
    value_display = "vals_bandit_" + pos
    location_display = "value_displayed_" + pos
    display_option = document.getElementById(location_display);
    // remove cued background color, display value, and remove it again
    display_option.style.background = "white";
    display_option.innerHTML = current_info[value_display][i][item_id];
    await sleep(display_info["presentation"]);
    display_option.style.background = "#26dabcde";
    display_option.innerHTML = "?";
    item_id += 1;
    display_free_choices("page5", item_id)
}


async function display_free_choices(old, item_id) {

    var [part, i, current_info] = progress_in_experiment();

    clickStart(old, "page5");

    if (item_id <= current_info["horizon"][i] + 11) {
        format_both_options("question")
        display_option_a.onclick = function () {
            next_value_free(i, item_id, current_info, 0)
        };
        display_option_b.onclick = function () {
            next_value_free(i, item_id, current_info, 1)
        };
        var n_remaining = ((current_info["horizon"][i] + 11) - item_id + 1);
        document.getElementById("n_remaining_choices").style.display = "block"
        document.getElementById("n_remaining_choices").innerHTML = "Nr. remaining choices = " + n_remaining
        if (n_remaining == 1) {
            document.getElementById("n_remaining_choices").style.color = "red"
        } else if (n_remaining <= 6) {
            document.getElementById("n_remaining_choices").style.color = "orange"
        } else {
            document.getElementById("n_remaining_choices").style.color = "green"
        }
    } else {
        document.getElementById("n_remaining_choices").style.color = "black"
        document.getElementById("n_remaining_choices").style.display = "none"
        update_trial_counter(part, i);
        if (part == 0 & i == (experiment_info["n_trials_practice"] - 1)) {
            // practice is over
            format_both_options("reset")
            clickStart("page5", "page8")
        } else if (part == 1 & i == (experiment_info["n_trials"] - 1)) {
            // experiment is over
            clickStart("page5", "page9")
        } else {
            // next trial
            format_both_options("reset")
            clickStart("page5", "page7")
        }
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
}

async function log_response(rt, i, part, stimulus_ids) {
    var x1_true = parseFloat(setup_expt["stimulus_info"]["x1_x2"][stimulus_ids[i]][0])
    var x2_true = parseFloat(setup_expt["stimulus_info"]["x1_x2"][stimulus_ids[i]][1])
    var x1_response = parseFloat(document.getElementById("myRange1").value)
    var x2_response = parseFloat(document.getElementById("myRange2").value)
    var x1_start = parseFloat(document.getElementById("myRange1_start").value)
    var x2_start = parseFloat(document.getElementById("myRange2_start").value)
    var data_store = {
        participant_id: participant_id,
        n_categories: n_categories,
        session: part,
        trial_id: i,
        x1_true: x1_true,
        x2_true: x2_true,
        x1_response: x1_response,
        x2_response: x2_response,
        x1_start: x1_start,
        x2_start: x2_start,
        rt: rt
    }
    var deviation = Math.sqrt(Math.pow((x1_true - x1_response), 2) + Math.pow((x2_true - x2_response), 2))
    document.getElementById("cr_deviation_cum").innerHTML = parseFloat(document.getElementById("cr_deviation_cum").innerHTML) + deviation

    var val1 = Math.floor(Math.random() * 100);
    var val2 = Math.floor(Math.random() * 100);
    document.getElementById("myRange1").value = val1;
    document.getElementById("myRange2").value = val2;
    document.getElementById("selected_monster").src = "stimuli/stimulus[" + val1 + "," + val2 + "].png"
    saveData(JSON.stringify(data_store), "cr");
}

async function my_link() {
    var rt = Date.now() - document.getElementById("time_var").innerHTML
    var i;
    part = parseInt(document.getElementById("part_reproduction").innerHTML)
    if (part == 0) {
        i = parseInt(document.getElementById("trial_nr_practice").innerHTML)
        stimulus_ids = setup_expt["trial_info"]["stimulus_id_rp"]
    }
    if (part == 1) {
        i = parseInt(document.getElementById("trial_nr_main").innerHTML)
        stimulus_ids = setup_expt["trial_info"]["stimulus_id_r1"]
    } else if (part == 2) {
        i = parseInt(document.getElementById("trial_nr_cr2").innerHTML)
        stimulus_ids = setup_expt["trial_info"]["stimulus_id_r2"]
    }

    if (i == total_trials0 & part == 0) { //practice
        log_response(rt, i, part, stimulus_ids);
        clickStart("page4", "page3.1")
        document.getElementById("part_reproduction").innerHTML = 1
    } else if (i == total_trials1 & part == 1) { //part 1 reproduction
        log_response(rt, i, part, stimulus_ids);
        clickStart("page4", "page6");
    } else if (i == total_trials2 & part == 2) { //part 2 reproduction
        log_response(rt, i, part, stimulus_ids);
        calculate_bonus("succeed")
        clickStart("page4", "page13");
    } else {
        log_response(rt, i, part, stimulus_ids);
        update_trial_counter(part, i)
        next_item_cr('page4');
    }
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

// continue with display categorization trial
// log categorization response

function wrap_categorization(old, i) {
    if (setup_expt["experiment_info"]["condition_id"] == 1) {//control
        next_item_control(old, i)
    } else if (setup_expt["experiment_info"]["condition_id"] == 2 | setup_expt["experiment_info"]["condition_id"] == 3) {
        next_item_cat(old, i)
    }
}


async function next_item_cat(old, i) {
    document.getElementById("cat_accuracy_running_mean").style.display = 'block';
    clickStart(old, 'page9')
    current_stim_id = stimulus_cat_trial[i]
    current_stim = stimulus_vals[current_stim_id]
    stim_path = "stimuli/stimulus[" + current_stim + "].png"
    mask_path = "stimuli/mask.png"
    // present stimuli and mask
    document.getElementById("item_displayed_cat").src = mask_path
    await sleep(setup_expt["display_info"]["categorization"]["iti"])
    document.getElementById("item_displayed_cat").src = "stimuli/fixcross.png"
    await sleep(setup_expt["display_info"]["categorization"]["fixcross"])
    document.getElementById("item_displayed_cat").src = stim_path
    document.getElementById("time_var").innerHTML = Date.now()
    document.addEventListener("keydown", handle_response, false);
}


async function handle_response(e) {
    var condition_id = parseInt(document.getElementById("condition_id").innerHTML)
    if (
        n_categories == 1 & (e.keyCode >= 49 && e.keyCode <= 52) ||
        n_categories == 1 & (e.keyCode >= 97 && e.keyCode <= 100) ||
        n_categories == 3 & (e.keyCode >= 49 && e.keyCode <= 51) ||
        n_categories == 3 & (e.keyCode >= 97 && e.keyCode <= 99) ||
        n_categories == 2 & (e.keyCode >= 49 && e.keyCode <= 50) ||
        n_categories == 2 & (e.keyCode >= 97 && e.keyCode <= 98) ||
        n_categories == 4 & (e.keyCode >= 49 && e.keyCode <= 52) ||
        n_categories == 4 & (e.keyCode >= 97 && e.keyCode <= 100)
    ) {
        var break_idx = parseInt(document.getElementById("break_idx").innerHTML)
        var str_frame = "timeframe" + Math.max(break_idx, 1)
        document.getElementById("item_displayed_cat").src = "stimuli/mask.png"

        var i = parseInt(document.getElementById("trial_nr_cat").innerHTML)
        var keyCode = e.keyCode;
        document.getElementById("key_id").innerHTML = keyCode;
        rt = Date.now() - document.getElementById("time_var").innerHTML;
        document.getElementById("rt").innerHTML = rt;

        document.removeEventListener("keydown", handle_response, false);
        cat_id_response = keycode_to_integer(keyCode)
        write_cat_results(i, cat_id_response)

        // responses within deadlinetime
        if (n_categories == 1 & rt <= setup_expt["display_info"]["categorization"]["deadlinetime"]) { // control
            var str = new String("Your response was: " + cat_id_response);
            document.getElementById("feedback_cat_true").innerHTML = str
            await sleep(setup_expt["display_info"]["categorization"]["feedbacktime_true"])
            document.getElementById("feedback_cat_true").innerHTML = ""
        } else if (n_categories > 1 & rt <= setup_expt["display_info"]["categorization"]["deadlinetime"]) {
            if (cat_id_response == category_id[i]) {
                document.getElementById("feedback_cat_true").innerHTML = "Well Done: " + category_name[category_id[i] - 1] + "!"
                await sleep(setup_expt["display_info"]["categorization"]["feedbacktime_true"])
                document.getElementById("feedback_cat_true").innerHTML = ""
            } else {
                var str = new String("Category would have been: " + category_name[category_id[i] - 1]);
                document.getElementById("feedback_cat_wrong").innerHTML = str
                await sleep(setup_expt["display_info"]["categorization"]["feedbacktime_wrong"])
                document.getElementById("feedback_cat_wrong").innerHTML = ""
            }
        }
        // responses given after deadlinetime
        if (rt > setup_expt["display_info"]["categorization"]["deadlinetime"]) {
            document.getElementById("feedback_cat_wrong").innerHTML = "Too slow, please respond faster!"
            await sleep(1000)
            document.getElementById("feedback_cat_wrong").innerHTML = ""
        }
        // reset cat_continued to 0 at a point > 60 secs after a break
        if ((i + 1) % Math.ceil(setup_expt["experiment_info"]["n_trials_categorization_total"] / 4) == 40) {
            document.getElementById("cat_continued").innerHTML = 0
        }
        // update trial counter
        document.getElementById("trial_nr_cat").innerHTML = i + 1

        // handle special timepoint in the experiment
        // end of categorization part
        if (i == setup_expt["experiment_info"]["n_trials_categorization_total"] - 1) {//1) {
            document.getElementById("cat_accuracy_running_mean").style.display = 'none';
            document.getElementById("part_reproduction").innerHTML = 2;
            document.getElementById("cat_continued").innerHTML = 1;
            cat_accuracies = calculate_categorization_accuracy(responses_cat_trial, setup_expt["experiment_info"]["n_trial_categorization_lag"])
            document.getElementById("cat_accuracy_overall").innerHTML = Math.round(100 * cat_accuracies[0]) + "%";
            document.getElementById("cat_accuracy_lag").innerHTML = Math.round(100 * cat_accuracies[1]) + "%";
            if (
                cat_accuracies[0] >= setup_expt["experiment_info"]["thx_cat_overall"] ||
                cat_accuracies[1] >= setup_expt["experiment_info"]["thx_cat_lag"] ||
                setup_expt["experiment_info"]["n_categories"] == 1
            ) {
                clickStart("page9", "page11")
            } else {
                calculate_bonus("dropout")
                clickStart("page9", "page14")
            }

        } // end of train-target trials
        else if (n_categories != 1 & i == setup_expt["experiment_info"]["n_trials_categorization_train_target"] - 1) {
            clickStart("page9", "page10b")

        } else if (
            // breaks after each quarter of the categorization trials
            (i + 1) % Math.ceil(setup_expt["experiment_info"]["n_trials_categorization_total"] / 4) == 0 &
            (i + 1) != setup_expt["experiment_info"]["n_trials_categorization_total"]
        ) {
            document.getElementById("break_idx").innerHTML = parseInt(document.getElementById("break_idx").innerHTML) + 1
            var break_idx = document.getElementById("break_idx").innerHTML
            trial_nr = i + 1
            document.getElementById("progress").innerHTML = "Your progress in the second part: " + trial_nr +
                " / " + setup_expt["experiment_info"]["n_trials_categorization_total"]
            clickStart("page9", "page10")
            str_countdown = "#time" + break_idx
            str_frame = "timeframe" + break_idx
            document.getElementById(str_frame).style.display = "block"
            var seconds = 60;
            var display = document.querySelector(str_countdown);
            startTimer(seconds, display);

            var timeout = setTimeout(function () {
                if (document.getElementById("cat_continued").innerHTML == 0) {
                    clickStart("page10", 'page9');
                    next_item_cat("page9")
                    document.getElementById("cat_continued").innerHTML = 1
                    document.getElementById(str_frame).style.display = "none"
                }
            }, 61000);

        } else {
            // default case continuing with next trial
            next_item_cat('page9', i + 1);
            document.getElementById(str_frame).style.display = "none"
        }
    }
}


function calculate_categorization_accuracy(responses_cat_trial, lag) {
    n_responses = responses_cat_trial.length
    sum_correct = responses_cat_trial.reduce((a, b) => a + b, 0)
    prop_correct_overall = ((parseFloat(sum_correct) / parseFloat(n_responses)))
    sum_correct_lag = responses_cat_trial.slice(n_responses - lag, n_responses).reduce((a, b) => a + b, 0)
    prop_correct_lag = ((parseFloat(sum_correct_lag) / parseFloat(lag)))
    cat_accuracies = [prop_correct_overall, prop_correct_lag]
    return cat_accuracies
}

function startTimer(duration, display) {
    var timer = duration, minutes, seconds;
    setInterval(function () {
        minutes = parseInt(timer / 60, 10);
        seconds = parseInt(timer % 60, 10);

        minutes = minutes < 10 ? "0" + minutes : minutes;
        seconds = seconds < 10 ? "0" + seconds : seconds;

        display.textContent = minutes + ":" + seconds;

        if (--timer < 0) {
            timer = duration;
        }
    }, 1000);
}


function keycode_to_integer(kc) {
    number_codes = {
        48: 0, 49: 1, 50: 2, 51: 3, 52: 4, 53: 5, 54: 6, 55: 7, 56: 8, 57: 9,
        96: 0, 97: 1, 98: 2, 99: 3, 100: 4, 101: 5, 102: 6, 103: 7, 104: 8, 105: 9
    }
    return number_codes[kc]
}


function write_cat_results(i, r) {
    condition_id = parseInt(document.getElementById("condition_id").innerHTML)
    if (n_categories == 1) {
        accuracy = 9999
    } else if (n_categories > 1) {
        accuracy = setup_expt["trial_info"]["category_id"][i] == r;
        var accuracy_int = accuracy | 0
        responses_cat_trial.push(accuracy_int)
    }
    var data_store = {
        participant_id: participant_id,
        n_categories: n_categories,
        trial_id: i,
        x1_true: setup_expt["stimulus_info"]["x1_x2"][setup_expt["trial_info"]["stimulus_id_c"][i]][0],
        x2_true: setup_expt["stimulus_info"]["x1_x2"][setup_expt["trial_info"]["stimulus_id_c"][i]][1],
        cat_true: setup_expt["trial_info"]["category_id"][i],
        response: r,
        accuracy: accuracy,
        rt: document.getElementById("rt").innerHTML
    }
    if (accuracy === true) {
        document.getElementById("cat_accuracy_cum").innerHTML = parseInt(document.getElementById("cat_accuracy_cum").innerHTML) + 1
    }
    document.getElementById("cat_accuracy_running_mean").innerHTML = "Average Accuracy = " +
        parseInt(100 * document.getElementById("cat_accuracy_cum").innerHTML / (i + 1)) + "%"
    //download(JSON.stringify(data_store), 'json.json', 'text/plain');
    saveData(JSON.stringify(data_store), "cat")
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
function instructioncheck(pg, pg_prev) {
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
        clickStart(pg, 'page3');
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


function set_category_instruction(n_categories) {
    var text;
    const text_3 = `There are two target categories and one non-target category.<br>One target category is called <b>Bukil</b>, the other target category is called <b>Venak</b>.<br>
    In the beginning of the experiment, you are presented with ` + setup_expt["experiment_info"]["n_trials_categorization_train_target"] +
        ` monsters only from the Bukil and Venak categories to get familiar with the two target categories.<br>
    After that phase, you are presented with all types of monsters.<br>
    After every response you are given feedback whether your response was correct or not accompanied by the true category name.<br><br>
    
    <b>Responding:</b><br>
    <b>Please try to respond within 3 seconds as accurately as possible.</b> You will get feedback to respond faster if you respond too slowly!<br>
    You can use the number keys on your keyboard to give a response in the task.<br>
    The numbers correspond to the respective category:<br>
    "1" on your keyboard corresponds to the non-target category.<br>
    "2" on your keyboard corresponds to the "Bukil" category.<br>
    "3" corresponds to the "Venak" category.<br><br>
    The next trial starts immediately after the feedback message has been displayed to you.`

    const text_2 = `There are two categories to be learned.<br>
    The names of the categories are <b>Bukil</b> and <b>Venak</b>.<br>
    Throughout the experiment you are going to see monsters from both categories.<br>
    Your goal is to learn to categorize the monsters into the respective category using feedback, which is provided after every response.<br>
    The feedback tells you whether your response was correct or not accompanied by the true category name.<br><br>

    <b>Important</b><br>
    Whether you make it to the third part / bonus round of the experiment depends on your performance in the categorization task.<br>
    You make it to the third part when your performance satisfies at least one of the two evaluation criteria:<br>
    First, if at least  ` +
        parseInt(100 * setup_expt["experiment_info"]["thx_cat_overall"]) + `
    of your categorization responses are correct.<br>
    Second, if at least ` +
        parseInt(100 * setup_expt["experiment_info"]["thx_cat_lag"]) + `
    of your categorization responses in the last ` +
        setup_expt["experiment_info"]["n_trial_categorization_lag"] + ` trials are correct.<br>
    The running average of your categorization accuracy is shown to you in the upper right corner of the screen throughout the categorization task.<br>

    <b> Responding:</b> <br>
    <b>Please try to respond within 3 seconds as accurately as possible.</b> You will get feedback to respond faster if you respond too slowly!<br>
    You can use the number keys on your keyboard to give a response in the task.<br>
    The numbers correspond to the respective category:<br>
    "1" on your keyboard corresponds to the non-target category.<br>
    "2" on your keyboard corresponds to the "Bukil" category.<br><br>
    The next trial starts immediately after the feedback message has been displayed to you.`

    const text_4 = `There are four categories to be learned.<br>
    The names of the categories are <b>Bukil</b>, <b>Venak</b>, <b>Monus<b>, and <b>Ladiv<b>.<br>
    Throughout the experiment you are going to see monsters from all four categories.<br>
    Your goal is to learn to categorize the monsters into the respective category using feedback, which is provided after every response.<br>
    The feedback tells you whether your response was correct or not accompanied by the true category name.<br><br>

    <b>Important</b><br>
    Whether you make it to the third part / bonus round of the experiment depends on your performance in the categorization task.<br>
    You make it to the third part when your performance satisfies at least one of the two evaluation criteria:<br>
    First, if at least  ` +
        parseInt(100 * setup_expt["experiment_info"]["thx_cat_overall"]) + `
    of your categorization responses are correct.<br>
    Second, if at least ` +
        parseInt(100 * setup_expt["experiment_info"]["thx_cat_lag"]) + `
    of your categorization responses in the last ` +
        setup_expt["experiment_info"]["n_trial_categorization_lag"] + ` trials are correct.<br>
        The running average of your categorization accuracy is shown to you in the upper right corner of the screen throughout the categorization task.<br>

    <b> Responding:</b> <br>
    <b>Please try to respond within 3 seconds as accurately as possible.</b> You will get feedback to respond faster if you respond too slowly!<br>
    You can use the number keys on your keyboard to give a response in the task.<br>
    The numbers correspond to the respective category:<br>
    "1" on your keyboard corresponds to the "Bukil" category.<br>
    "2" on your keyboard corresponds to the "Venak" category.<br>
    "3" on your keyboard corresponds to the "Monus" category.<br>
    "4" on your keyboard corresponds to the "Ladiv" category.<br><br>
    The next trial starts immediately after the feedback message has been displayed to you.`

    if (n_categories == 2) {
        text = text_2
    } else if (n_categories == 3) {
        text = text_3
    } else if (n_categories == 1) {
        text = ""
    } else if (n_categories == 4) {
        text = text_4
    }
    return (text)
}


function load_csv() {
    var txt = d3.json("rotate-conditions.json", function (data) {
        var condition_counts;
        //condition_counts = Object.values(data);
        condition_counts = Object.keys(data).map(function (e) {
            return data[e]
        })
        var max_counts = 9999999999;
        for (var i = 0; i < condition_counts.length; i++) {
            var obj = condition_counts[i]
            if (obj < max_counts) {
                max_counts = obj
                condition_id = [1, 2, 3][i]
            }
        }
        n_categories = [1, 2, 3][(condition_id % 3)]
        const str_idx = "condition" + condition_id
        data[str_idx] += 1;
        document.getElementById("condition_id").innerHTML = condition_id
        document.getElementById("n_categories").innerHTML = n_categories
        saveConditions(JSON.stringify(data));
    });
    clickStart('page0', 'page1')
}


function condition_and_ncategories() {
    n_different_categories = 3;
    var condition_id = Math.ceil(Math.random() * n_different_categories);
    var n_categories = [1, 2, 4][(condition_id % n_different_categories)] // similarity, ellipse, & squares
    condition_id = 3
    n_categories = 4
    document.getElementById("condition_id").innerHTML = condition_id
    document.getElementById("n_categories").innerHTML = n_categories
    if (n_categories == 1) {
        textCond = `<b>Similarity:</b> Your task in the second part will be somewhat different.<br>
                                You are asked to judge how similar the monster presented on the current trial is to the monster presented on the previous trial.<br>
                                To do so, use the numbers from 1-4 on the keyboard.<br>
                                When the monster is exactly the same as on the previous trial, press 4.<br>
                                When the monster looks very different (i.e., head and belly differ a lot), press 1.<br>
                                <b> --> Takes approx. 45 min</b><br>
                                There will be breaks in between these 45 mins.`
    } else {
        textCond = `<b>Categorization:</b> The monsters all look somewhat similar, but they come from different tribes.<br>
                                Use the information about the spikiness of their head and the fill of their belly to gauge what tribes they are from.<br>
                                Give your response on a trial using the digit keys on your keyboard.For example, digit key "1" relates to category nr. 1.<br>
                                You are going to get a feedback after every trial, which may help getting you started.<br>
                                <b> --> Takes approx. 45 min</b><br>
                                There will be breaks in between these 45 mins.`
    }
    document.getElementById("taskTextCondition").innerHTML = textCond;
    clickStart('page0', 'page1')
}


function saveConditions(filedata) {
    var filename = "rotate-conditions.json";
    $.post("overwrite_data.php", { postresult: filedata + "\n", postfile: filename })
}


function set_main_vars(condition_id) {
    setup_expt = setup_experiment(condition_id);
    instruction_category = set_category_instruction(setup_expt["experiment_info"]["n_categories"])

    var x = document.getElementById("page7");
    x.querySelector(".category_instruction").innerHTML = instruction_category;;

    (() => {
        document.getElementById("myText").innerHTML = document.getElementById("n_categories").innerHTML;
    })();
    stimulus_crp_trial = setup_expt["trial_info"]["stimulus_id_rp"]
    stimulus_cr1_trial = setup_expt["trial_info"]["stimulus_id_r1"]
    stimulus_cr2_trial = setup_expt["trial_info"]["stimulus_id_r2"]
    stimulus_cat_trial = setup_expt["trial_info"]["stimulus_id_c"]
    responses_cat_trial = setup_expt["trial_info"]["response_c"]
    category_id = setup_expt["trial_info"]["category_id"]
    category_name = setup_expt["stimulus_info"]["category_name"]
    stimulus_vals = setup_expt["stimulus_info"]["x1_x2"]
    total_trials0 = setup_expt["experiment_info"]["n_practice_reproduction"] - 1
    total_trials1 = setup_expt["experiment_info"]["n_trials_reproduction_1"] - 1
    total_trials2 = setup_expt["experiment_info"]["n_trials_reproduction_2"] - 1;
}


function saveBonus(filedata) {
    var filename = "./data/bonus.json";
    $.post("save_data.php", { postresult: filedata + "\n", postfile: filename })
}


function calculate_bonus(flag_performance) {
    if (flag_performance == "dropout") {
        var bonus_store = {
            participant_id: participant_id,
            bonus_cr: 0,
            bonus_cat: 0,
            bonus_total: 0
        }
    } else if (flag_performance == "succeed") {
        // bonus continuous reproduction
        const bonus_cr_max = 3.60
        var n_trials_reproduction = setup_expt["experiment_info"]["n_trials_reproduction_1"] + setup_expt["experiment_info"]["n_trials_reproduction_2"]
        var avg_deviation = parseFloat(document.getElementById("cr_deviation_cum").innerHTML) / n_trials_reproduction
        var coef_bonus = Math.min(51, avg_deviation)
        var above_chance = 51 - coef_bonus
        var prop_bonus = above_chance / 46 // anything closer than 5 from the target counts as "perfect"
        var bonus_cr = Math.round((prop_bonus * bonus_cr_max * 100)) / 100

        // bonus categorization
        var bonus_cat;
        if (setup_expt["experiment_info"]["n_categories"] == 1) {
            bonus_cat = 1.80
        } else {
            const bonus_cat_max = 3.60
            var n_trials_categorization = setup_expt["experiment_info"]["n_trials_categorization_total"]
            var prop_correct_cat = parseInt(document.getElementById("cat_accuracy_cum").innerHTML) / n_trials_categorization
            bonus_cat = Math.round((prop_correct_cat * bonus_cat_max * 100)) / 100
        }
        if (bonus_cat < 1.8) { bonus_cat = 1.8 }
        if (bonus_cr < 1.8) { bonus_cr = 1.8 }

        var bonus_total = Math.round((bonus_cr + bonus_cat) * 100) / 100;


        var bonus_store = {
            participant_id: participant_id,
            bonus_cr: bonus_cr,
            bonus_cat: bonus_cat,
            bonus_total: bonus_total
        }
    }

    saveBonus(JSON.stringify(bonus_store));

    (() => {
        document.getElementById("total_bonus").innerHTML = bonus_total;
        document.getElementById("cr_bonus").innerHTML = bonus_cr;
        document.getElementById("cat_bonus").innerHTML = bonus_cat;
    })();

}

function redirect_to_prolific() {
    window.location.href = "https://app.prolific.co/submissions/complete?cc=240D34C0";
}
