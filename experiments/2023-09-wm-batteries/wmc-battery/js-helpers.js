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


function saveData(filedata, task) {
    var filename = "./data/" + task + "-participant-" + participant_id + ".json";
    $.post("save_data.php", { postresult: filedata + "\n", postfile: filename })
}

async function saveSeveralData(filedata, task) {
    var filename = "./data/" + task + "-participant-" + participant_id + ".json";
    var n_data = filedata.length;
    for (var i = 0; i < n_data; i++) {
        $.post("save_data.php", { postresult: JSON.stringify(filedata[i]) + "\n", postfile: filename })
    }
}


function download(content, fileName, contentType) {
    var a = document.createElement("a");
    var file = new Blob([content], { type: contentType });
    a.href = URL.createObjectURL(file);
    a.download = fileName;
    a.click();
}

function prepare_recall(data_recall) {
    var trial_id_recall = data_recall.select("trial_id_recall");
    var set_size = data_recall.select("set_size");
    var stimuli = data_recall.select("stimuli");
    var responses = data_recall.select("recall");
    var n_correct = data_recall.select("accuracy");
    var rt = data_recall.select("rt");
    var data_recall_clean = {
        participant_id: participant_id,
        trial_id_recall: trial_id_recall,
        set_size: set_size,
        stimuli: stimuli,
        response: responses,
        n_correct: n_correct,
        rt: rt
    };
    return (data_recall_clean)
}

function prepare_processing(data_processing) {
    var trial_id_recall = data_processing.select("trial_id_recall");
    var trial_id_processing = data_processing.select("trial_id_processing");
    var set_size = data_processing.select("set_size");
    var accuracy = data_processing.select("accuracy");
    var rt = data_processing.select("rt");
    var data_clean = {
        trial_id_recall: trial_id_recall,
        trial_id_processing: trial_id_processing,
        set_size: set_size,
        accuracy: accuracy,
        rt: rt
    };
    return (data_clean)
}

function make_rect(x_start, y_start) {
    const rect_object = {
        obj_type: 'rect',
        startX: x_start, // location of the rectangle's center in the canvas
        startY: y_start,
        width: 200,
        height: 100,
        line_color: 'black', // You can use the HTML color name instead of the HEX color.
        line_width: 3,
        show_start_time: 0, // ms after the start of the trial
        show_end_time: 15000
    }
    return (rect_object);
}


var eqsCorrect = [
    [true, true, false, true, false, false, false, false, false, false, true, true, true, true, false, false, true, false, false, true, true, true, true, true, true, false, true, true, true, false, true, false, true, true, false, true, false, true, false, false, true, false, false, false, true, false, true, false, false, false, true, true, true, false, true, true, true, false, false, true, true, false, false, true, false, true, true, false, true, true, true, true, false, true, true, false, false, true, false, true, true, false, false, true, true, false, true, true, false, false, true, false, true, true, true, true, false, true, true, true, false, false, false, false, true, true, true, true, false, true, true, false, true, true, true, false, false, false, true, false, false, false, true, true, false, false, false, true, false, false, false, true, false, false, false],
    [false, false, true, false, false, false, false, false, false, false, true, false, true, true, true, true, false, true, false, false, true, false, false, true, false, true, false, true, true, false, true, true, true, false, true, true, false, true, false, true, true, false, true, true, true, false, true, true, false, true, true, true, false, false, false, true, true, true, true, true, true, false, false, false, false, false, false, true, true, false, true, true, true, true, false, true, true, false, false, false, false, true, false, false, false, false, false, true, true, true, true, false, false, true, false, true, false, false, true, false, false, false, true, false, false, false, false, false, false, false, true, false, false, false, false, false, false, true, false, true, true, false, false, false, false, false, false, true, false, true, true, false, false, true, true]
]
var eqsCorrect_demo = [
    [true, false, true, false, true, false, false, true, true, true, true, false, false, true, true, true, true, true, true, false, false, false, true, true, true, false, false, true, false, true, true, true, true, false, false, false, true, true, false, true, true, false, true, false, true, false, true, false, false, true, false, true, false, false, false, true, true, true, true, false, true, true, true, false, true, false, false, true, false, true, false, true, false, true, true, false, false, false, true, false, true, true, true, false, false, true, true, false, false, true, true, true, false, true, true, true, false, false, true, true],
    [false, true, true, false, false, true, true, true, true, true, true, true, false, false, false, true, true, false, false, true, false, false, false, false, false, false, false, false, false, true, false, true, false, true, true, true, false, true, true, true, true, false, false, true, false, false, true, true, true, false, false, false, false, false, true, true, false, false, true, true, false, false, true, false, true, false, true, true, true, false, false, false, false, false, false, true, true, true, true, true, false, true, false, true, true, true, true, true, true, true, false, false, false, false, true, true, false, true, false, false]
];
var operations = [
    [' + ', ' - ', ' + ', ' + ', ' - ', ' + ', ' + ', ' - ', ' - ', ' + ', ' + ', ' + ', ' - ', ' + ', ' - ', ' - ', ' - ', ' + ', ' + ', ' - ', ' + ', ' + ', ' - ', ' + ', ' - ', ' + ', ' - ', ' + ', ' - ', ' - ', ' + ', ' - ', ' - ', ' + ', ' + ', ' - ', ' - ', ' + ', ' - ', ' - ', ' + ', ' - ', ' + ', ' + ', ' - ', ' - ', ' - ', ' + ', ' - ', ' - ', ' - ', ' - ', ' - ', ' - ', ' + ', ' + ', ' + ', ' + ', ' + ', ' + ', ' + ', ' - ', ' + ', ' + ', ' + ', ' + ', ' - ', ' + ', ' + ', ' + ', ' - ', ' + ', ' + ', ' - ', ' + ', ' + ', ' - ', ' + ', ' + ', ' + ', ' + ', ' + ', ' + ', ' - ', ' - ', ' - ', ' + ', ' + ', ' + ', ' + ', ' + ', ' + ', ' + ', ' + ', ' + ', ' - ', ' + ', ' + ', ' + ', ' - ', ' - ', ' + ', ' - ', ' - ', ' - ', ' - ', ' - ', ' + ', ' - ', ' - ', ' - ', ' + ', ' - ', ' - ', ' + ', ' - ', ' - ', ' - ', ' - ', ' + ', ' - ', ' - ', ' + ', ' + ', ' - ', ' - ', ' - ', ' + ', ' - ', ' + ', ' + ', ' + ', ' - ', ' - ', ' + '],
    [' + ', ' + ', ' - ', ' + ', ' + ', ' + ', ' + ', ' + ', ' - ', ' + ', ' - ', ' - ', ' + ', ' + ', ' + ', ' + ', ' + ', ' + ', ' + ', ' + ', ' + ', ' - ', ' + ', ' + ', ' - ', ' - ', ' - ', ' - ', ' - ', ' - ', ' + ', ' - ', ' - ', ' + ', ' - ', ' + ', ' - ', ' - ', ' + ', ' - ', ' + ', ' + ', ' + ', ' + ', ' - ', ' - ', ' + ', ' + ', ' - ', ' - ', ' - ', ' + ', ' - ', ' + ', ' + ', ' - ', ' + ', ' + ', ' + ', ' + ', ' - ', ' + ', ' - ', ' - ', ' - ', ' + ', ' - ', ' + ', ' + ', ' + ', ' - ', ' - ', ' + ', ' - ', ' - ', ' + ', ' + ', ' + ', ' + ', ' - ', ' - ', ' - ', ' + ', ' - ', ' - ', ' + ', ' + ', ' - ', ' - ', ' - ', ' + ', ' + ', ' + ', ' + ', ' - ', ' - ', ' + ', ' + ', ' - ', ' - ', ' + ', ' + ', ' - ', ' + ', ' + ', ' - ', ' + ', ' + ', ' + ', ' + ', ' - ', ' + ', ' - ', ' + ', ' - ', ' - ', ' + ', ' - ', ' - ', ' - ', ' - ', ' - ', ' + ', ' + ', ' + ', ' + ', ' - ', ' - ', ' - ', ' - ', ' + ', ' + ', ' + ', ' - ', ' + ']
];
var ansDiffs = [
    [1, 2, 2, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 2, 1, 2, 1, 1, 2, 1, 1, 1, 2, 2, 1, 2, 1, 2, 1, 1, 1, 2, 2, 2, 1, 2, 1, 1, 2, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1, 1, 1, 1],
    [1, 2, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2, 1, 2, 1, 2, 2, 1, 2, 1, 2, 1, 1, 1, 2, 1, 2, 1, 2, 2, 1, 1, 2, 1, 1, 1, 1, 2, 1, 2, 2, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 2, 1, 2, 2, 1, 2, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 1, 2, 1, 1, 2, 1, 1, 2, 1, 2, 2, 1, 1, 1, 2, 2, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 2, 2, 1, 2, 2, 2, 1, 2, 2, 2, 1, 1, 2]
];
var coinFlips = [
    [false, true, true, false, false, true, false, false, true, false, true, true, false, true, false, true, false, true, true, false, true, false, true, true, true, false, false, false, true, false, false, false, false, false, true, false, false, false, false, true, true, true, true, true, false, true, true, true, false, true, true, false, false, true, false, true, false, false, true, false, true, true, true, true, false, true, true, false, true, true, true, true, false, false, true, false, false, false, true, true, true, true, false, true, false, false, false, false, false, true, false, false, false, true, false, true, false, true, true, true, true, false, false, true, false, true, true, true, true, false, false, false, true, true, false, true, true, true, true, true, true, true, false, false, true, true, true, true, true, true, false, true, false, false, true],
    [true, false, false, false, false, false, true, false, false, true, true, true, false, true, true, false, false, false, false, true, true, true, true, false, true, true, false, true, true, true, true, true, false, true, false, true, false, true, false, true, true, true, false, false, true, true, true, false, false, false, true, false, true, true, false, true, false, true, false, false, true, false, true, false, false, false, false, false, false, false, false, false, true, false, true, true, true, true, false, false, true, true, true, true, true, false, true, true, false, false, true, true, true, false, true, true, false, false, false, true, false, true, false, false, true, false, true, false, false, false, false, false, true, true, false, true, false, false, false, false, false, true, false, true, false, true, false, true, false, false, false, false, true, true, true]
];
var num1plus = [
    [4, 6, 6, 8, 4, 2, 9, 6, 1, 6, 1, 2, 1, 2, 8, 8, 7, 2, 6, 10, 3, 8, 7, 7, 5, 6, 7, 2, 1, 6, 5, 3, 1, 9, 2, 1, 10, 8, 4, 8, 4, 4, 6, 3, 10, 10, 4, 1, 5, 8, 2, 2, 6, 2, 9, 5, 2, 7, 4, 9, 5, 8, 2, 10, 1, 8, 3, 2, 3, 9, 10, 10, 2, 4, 4, 1, 10, 2, 3, 1, 1, 9, 5, 8, 10, 8, 3, 7, 10, 9, 10, 1, 2, 9, 7, 5, 2, 5, 9, 1, 7, 4, 5, 1, 1, 6, 6, 2, 1, 9, 6, 1, 2, 1, 5, 8, 6, 6, 7, 4, 8, 5, 8, 1, 2, 7, 5, 7, 4, 10, 6, 5, 3, 8, 1],
    [5, 5, 4, 7, 8, 1, 4, 6, 7, 1, 5, 2, 2, 3, 10, 8, 5, 9, 10, 1, 4, 2, 8, 3, 2, 10, 4, 2, 4, 6, 2, 8, 6, 5, 4, 7, 9, 2, 9, 9, 6, 9, 2, 1, 10, 6, 8, 2, 4, 9, 10, 3, 4, 2, 2, 1, 8, 7, 9, 10, 7, 3, 8, 9, 6, 3, 4, 5, 4, 6, 2, 2, 5, 6, 7, 7, 3, 3, 9, 2, 2, 5, 8, 4, 10, 1, 2, 3, 9, 4, 6, 6, 9, 10, 10, 7, 2, 4, 2, 1, 8, 4, 9, 6, 3, 1, 6, 5, 4, 8, 4, 1, 8, 8, 10, 3, 6, 9, 8, 1, 10, 1, 3, 9, 2, 2, 10, 4, 1, 2, 8, 9, 3, 6, 5]
];
var num2plus = [
    [2, 6, 5, 2, 5, 9, 8, 8, 1, 5, 8, 9, 10, 8, 10, 6, 5, 7, 6, 4, 2, 6, 8, 1, 5, 4, 6, 4, 3, 1, 4, 2, 7, 7, 3, 10, 1, 1, 5, 5, 4, 8, 9, 5, 4, 7, 5, 6, 2, 4, 2, 3, 4, 9, 10, 9, 7, 7, 1, 5, 2, 9, 6, 4, 6, 5, 6, 8, 5, 6, 7, 5, 5, 5, 8, 4, 2, 10, 2, 8, 3, 3, 7, 4, 6, 4, 6, 10, 3, 7, 9, 5, 3, 9, 5, 5, 3, 10, 5, 10, 7, 6, 2, 4, 7, 6, 2, 5, 9, 6, 7, 4, 6, 9, 9, 6, 1, 3, 7, 9, 8, 4, 7, 2, 10, 10, 10, 5, 5, 9, 4, 6, 5, 5, 2],
    [7, 1, 1, 7, 10, 2, 2, 2, 3, 10, 2, 8, 7, 7, 2, 4, 4, 7, 6, 5, 2, 7, 3, 2, 9, 5, 4, 5, 6, 1, 2, 2, 9, 6, 7, 5, 4, 9, 5, 7, 8, 2, 3, 4, 4, 3, 5, 4, 1, 1, 3, 1, 6, 9, 5, 8, 2, 9, 3, 1, 2, 2, 9, 2, 8, 9, 5, 10, 5, 10, 9, 2, 3, 6, 9, 8, 2, 8, 10, 10, 3, 2, 7, 8, 1, 9, 9, 1, 3, 1, 1, 3, 6, 9, 4, 9, 5, 3, 8, 9, 4, 5, 4, 9, 7, 1, 3, 5, 8, 3, 5, 6, 2, 10, 5, 7, 5, 1, 3, 6, 4, 9, 2, 6, 3, 1, 2, 9, 3, 2, 10, 10, 5, 1, 6]
];
var num1minus = [
    [8, 6, 8, 6, 7, 5, 8, 9, 1, 9, 7, 1, 2, 8, 8, 5, 10, 1, 6, 8, 9, 5, 2, 6, 4, 9, 2, 7, 6, 6, 3, 1, 3, 3, 4, 3, 4, 9, 8, 8, 4, 3, 10, 1, 4, 10, 6, 3, 7, 6, 6, 4, 8, 1, 9, 5, 2, 6, 4, 1, 7, 5, 6, 4, 8, 3, 8, 10, 6, 2, 3, 7, 7, 1, 6, 9, 7, 9, 5, 3, 5, 8, 4, 10, 4, 9, 5, 3, 5, 7, 8, 7, 6, 9, 9, 10, 10, 8, 1, 6, 8, 2, 8, 6, 10, 1, 7, 7, 6, 1, 9, 1, 2, 10, 4, 10, 5, 2, 9, 2, 9, 1, 8, 9, 5, 1, 1, 6, 1, 7, 1, 3, 4, 1, 7],
    [8, 3, 6, 7, 8, 9, 4, 7, 7, 8, 5, 6, 8, 2, 4, 10, 10, 3, 5, 9, 7, 1, 6, 1, 6, 8, 5, 5, 2, 3, 10, 2, 5, 10, 3, 10, 2, 7, 6, 8, 7, 7, 6, 4, 4, 2, 6, 10, 4, 6, 4, 10, 9, 1, 3, 9, 1, 10, 3, 5, 2, 1, 7, 3, 3, 9, 5, 7, 2, 2, 6, 7, 1, 6, 4, 1, 5, 8, 3, 1, 7, 1, 2, 1, 1, 7, 6, 9, 4, 8, 2, 8, 6, 5, 2, 8, 6, 3, 6, 4, 6, 1, 9, 10, 4, 3, 6, 5, 1, 10, 2, 9, 8, 7, 3, 6, 3, 10, 7, 6, 4, 4, 2, 1, 4, 9, 4, 4, 2, 8, 6, 9, 9, 7, 7]
];
var num2minus = [
    [1, 5, 2, 1, 4, 6, 1, 2, 1, 1, 2, 3, 2, 1, 1, 2, 1, 2, 2, 1, 1, 1, 5, 1, 3, 1, 2, 1, 1, 1, 3, 1, 2, 2, 2, 3, 1, 1, 3, 2, 1, 7, 8, 1, 3, 6, 2, 1, 1, 1, 1, 2, 1, 3, 7, 4, 2, 6, 1, 3, 1, 4, 1, 3, 4, 2, 4, 6, 2, 1, 1, 2, 1, 4, 4, 2, 1, 7, 1, 7, 2, 2, 3, 3, 5, 2, 2, 6, 1, 3, 8, 4, 2, 2, 3, 1, 1, 9, 4, 4, 2, 1, 1, 1, 5, 4, 1, 1, 5, 5, 1, 3, 3, 3, 6, 2, 1, 2, 1, 2, 6, 3, 1, 1, 3, 9, 6, 3, 1, 1, 1, 3, 4, 2, 1],
    [4, 1, 1, 5, 7, 1, 1, 1, 1, 4, 1, 7, 5, 2, 1, 2, 2, 3, 3, 3, 1, 6, 2, 1, 2, 3, 3, 4, 2, 1, 1, 1, 6, 2, 1, 4, 1, 3, 4, 2, 4, 1, 1, 3, 3, 1, 4, 2, 1, 1, 1, 1, 2, 4, 2, 7, 1, 8, 2, 1, 1, 1, 3, 1, 5, 6, 2, 6, 4, 3, 2, 1, 2, 5, 3, 6, 1, 2, 7, 6, 1, 1, 4, 4, 1, 2, 4, 1, 1, 1, 1, 1, 2, 6, 3, 7, 3, 2, 5, 1, 1, 2, 3, 5, 1, 1, 1, 2, 4, 2, 1, 3, 1, 9, 4, 2, 1, 1, 1, 1, 2, 1, 1, 2, 2, 1, 1, 2, 1, 1, 2, 9, 3, 1, 3]
];
var SetSizes_prep = [
    [8, 7, 8, 4, 6, 7, 6, 5, 4, 8, 4, 6, 7, 5, 5],
    [7, 6, 6, 5, 8, 7, 8, 7, 8, 4, 4, 5, 6, 5, 4]
];
var letterlists_prep = [
    [['Z', 'K', 'M', 'S', 'V', 'P', 'D', 'R'], ['B', 'S', 'T', 'D', 'N', 'Z', 'P'], ['B', 'L', 'M', 'C', 'D', 'R', 'S', 'G'], ['T', 'H', 'Z', 'C'], ['P', 'X', 'T', 'D', 'F', 'L'], ['D', 'H', 'G', 'W', 'C', 'M', 'B'], ['Z', 'M', 'P', 'N', 'C', 'G'], ['B', 'N', 'L', 'K', 'S'], ['V', 'P', 'H', 'C'], ['F', 'S', 'P', 'B', 'W', 'H', 'D', 'R'], ['D', 'C', 'W', 'F'], ['H', 'X', 'M', 'B', 'D', 'L'], ['N', 'D', 'K', 'S', 'T', 'C', 'Z'], ['D', 'F', 'R', 'C', 'S'], ['N', 'Z', 'K', 'T', 'R']],
    [['W', 'D', 'L', 'K', 'F', 'V', 'G'], ['V', 'Z', 'C', 'D', 'S', 'R'], ['F', 'S', 'K', 'N', 'P', 'B'], ['L', 'D', 'R', 'P', 'M'], ['R', 'D', 'S', 'B', 'X', 'T', 'H', 'N'], ['Z', 'C', 'V', 'D', 'F', 'M', 'W'], ['S', 'R', 'B', 'K', 'T', 'H', 'Z', 'F'], ['P', 'W', 'N', 'F', 'H', 'D', 'K'], ['B', 'C', 'R', 'P', 'W', 'M', 'D', 'N'], ['C', 'H', 'W', 'L'], ['F', 'K', 'L', 'R'], ['S', 'Z', 'D', 'F', 'H'], ['W', 'B', 'N', 'X', 'M', 'Z'], ['F', 'D', 'X', 'C', 'K'], ['L', 'V', 'G', 'K']]
];


var setSizes_symm = [
    [3, 4, 4, 3, 3, 4, 4, 3, 5, 5, 2, 4, 4, 3, 2, 5, 3, 2]
];

var blackBoxNumbers = [
    [17, 17, 17, 19, 19, 19, 17, 18, 17, 19, 19, 18, 19, 19, 18, 18, 18, 17, 17, 19, 19, 19, 17, 19, 19, 19, 18, 18, 19, 17, 19, 17, 17, 18, 17, 19, 19, 17, 18, 17, 17, 19, 18, 17, 17, 19, 17, 19, 17, 17, 18, 19, 19, 19, 18, 18, 17, 19, 17, 17, 19, 18, 18]
];

var blackBoxesAll = [
    [[[7, 3], [4, 2], [4, 1], [6, 2], [3, 3], [7, 2], [1, 2], [0, 3], [4, 0], [6, 3], [7, 1], [5, 2], [3, 0], [3, 2], [4, 3], [3, 1], [5, 0]], [[3, 2], [2, 3], [1, 0], [4, 0], [5, 0], [2, 0], [3, 3], [2, 2], [4, 2], [6, 2], [0, 3], [4, 1], [0, 2], [2, 1], [4, 3], [7, 0], [1, 1]], [[3, 3], [2, 1], [3, 2], [5, 0], [6, 3], [6, 2], [0, 2], [7, 3], [7, 0], [2, 3], [0, 1], [6, 0], [1, 2], [3, 0], [7, 2], [3, 1], [5, 1]], [[3, 3], [0, 0], [6, 1], [4, 3], [7, 0], [5, 3], [0, 2], [7, 3], [6, 3], [6, 0], [5, 1], [6, 2], [2, 3], [3, 2], [1, 2], [2, 2], [7, 2], [0, 1], [3, 0]], [[7, 0], [6, 2], [1, 1], [4, 1], [1, 2], [3, 3], [2, 3], [1, 3], [4, 2], [0, 0], [2, 0], [4, 3], [6, 1], [6, 3], [6, 0], [5, 2], [5, 3], [7, 2], [3, 0]], [[7, 0], [7, 2], [1, 0], [2, 1], [4, 0], [3, 1], [3, 3], [5, 0], [1, 3], [6, 1], [1, 1], [6, 2], [2, 0], [5, 2], [3, 2], [7, 3], [4, 1], [1, 2], [4, 2]], [[6, 1], [5, 0], [4, 3], [2, 2], [2, 0], [1, 3], [5, 2], [4, 0], [3, 3], [7, 2], [0, 2], [1, 0], [1, 2], [3, 0], [3, 1], [0, 0], [6, 3]], [[6, 2], [6, 1], [1, 2], [7, 2], [6, 0], [5, 2], [5, 3], [3, 1], [2, 1], [7, 1], [5, 1], [0, 0], [2, 3], [0, 1], [3, 0], [0, 2], [3, 2], [7, 0]], [[5, 2], [7, 1], [7, 0], [4, 3], [0, 2], [3, 0], [7, 2], [6, 1], [3, 1], [2, 0], [3, 3], [6, 2], [2, 3], [6, 0], [0, 1], [2, 1], [1, 1]], [[2, 1], [5, 1], [7, 0], [4, 3], [6, 0], [6, 1], [3, 3], [6, 3], [5, 2], [4, 1], [3, 0], [6, 2], [5, 0], [0, 1], [2, 3], [3, 1], [0, 2], [0, 0], [7, 1]], [[6, 1], [0, 3], [0, 2], [7, 0], [4, 2], [1, 0], [3, 1], [4, 1], [5, 1], [2, 0], [1, 2], [1, 3], [7, 2], [6, 2], [7, 3], [6, 0], [4, 0], [2, 3], [5, 2]], [[6, 2], [3, 0], [7, 3], [4, 2], [2, 2], [0, 3], [7, 0], [5, 0], [3, 3], [1, 2], [1, 3], [3, 2], [5, 2], [4, 1], [5, 1], [4, 0], [0, 2], [0, 0]], [[2, 1], [7, 2], [4, 1], [6, 0], [0, 0], [3, 3], [4, 2], [1, 0], [1, 1], [6, 2], [2, 2], [4, 3], [6, 1], [7, 3], [7, 0], [0, 3], [7, 1], [3, 0], [3, 1]], [[2, 2], [5, 1], [4, 0], [3, 3], [2, 3], [1, 3], [3, 2], [1, 2], [4, 1], [0, 1], [0, 0], [4, 2], [3, 1], [5, 0], [1, 1], [5, 2], [4, 3], [2, 1], [6, 1]], [[5, 0], [3, 3], [0, 1], [4, 1], [4, 2], [6, 2], [2, 1], [4, 0], [0, 0], [6, 0], [3, 1], [1, 2], [3, 0], [2, 3], [0, 3], [2, 2], [0, 2], [1, 1]], [[7, 1], [0, 3], [7, 3], [0, 0], [7, 2], [4, 2], [4, 0], [3, 0], [7, 0], [2, 3], [5, 3], [2, 1], [0, 1], [4, 1], [1, 0], [3, 1], [5, 2], [6, 0]], [[0, 2], [4, 1], [6, 2], [2, 0], [3, 1], [5, 2], [3, 2], [2, 1], [4, 0], [1, 2], [5, 3], [1, 3], [1, 0], [6, 3], [4, 3], [3, 3], [7, 3], [6, 1]], [[3, 3], [2, 3], [4, 2], [2, 0], [5, 1], [6, 3], [4, 1], [6, 0], [2, 1], [4, 3], [6, 2], [0, 3], [7, 2], [3, 0], [0, 2], [5, 0], [2, 2]], [[3, 0], [3, 3], [0, 0], [3, 1], [1, 3], [7, 0], [3, 2], [2, 1], [7, 3], [5, 1], [5, 3], [5, 2], [4, 3], [2, 0], [2, 3], [2, 2], [6, 3]], [[7, 3], [1, 0], [3, 2], [6, 1], [4, 1], [2, 2], [4, 2], [6, 2], [5, 0], [5, 2], [4, 0], [4, 3], [3, 1], [6, 0], [5, 3], [7, 1], [3, 0], [1, 3], [5, 1]], [[5, 2], [6, 0], [3, 1], [7, 3], [4, 1], [6, 1], [5, 3], [0, 2], [7, 1], [2, 1], [3, 2], [0, 1], [1, 0], [3, 0], [4, 3], [7, 0], [1, 3], [2, 0], [5, 0]], [[7, 2], [0, 0], [4, 2], [5, 0], [7, 1], [5, 3], [0, 3], [6, 0], [6, 1], [2, 0], [7, 3], [6, 2], [3, 1], [0, 2], [6, 3], [3, 0], [2, 3], [1, 3], [4, 1]], [[5, 3], [1, 1], [0, 1], [3, 2], [2, 0], [6, 1], [6, 0], [7, 2], [7, 0], [4, 1], [3, 3], [6, 2], [1, 2], [5, 0], [1, 0], [4, 0], [2, 1]], [[0, 1], [4, 3], [3, 0], [7, 0], [5, 3], [0, 3], [2, 2], [7, 3], [2, 1], [6, 2], [1, 0], [6, 0], [4, 0], [5, 1], [1, 3], [4, 1], [0, 0], [2, 0], [7, 1]], [[7, 2], [4, 3], [6, 0], [3, 0], [5, 1], [1, 2], [3, 1], [3, 2], [4, 1], [4, 0], [0, 0], [6, 3], [0, 2], [3, 3], [0, 3], [2, 0], [2, 2], [6, 1], [7, 0]], [[1, 3], [2, 2], [5, 0], [5, 1], [0, 3], [4, 1], [7, 3], [0, 1], [2, 0], [3, 2], [1, 2], [6, 1], [1, 1], [4, 3], [7, 1], [3, 0], [4, 0], [6, 2], [5, 2]], [[5, 0], [7, 1], [3, 2], [7, 0], [6, 2], [5, 2], [0, 1], [5, 3], [0, 2], [6, 1], [3, 1], [2, 1], [0, 0], [3, 0], [5, 1], [4, 3], [6, 0], [2, 0]], [[2, 2], [3, 0], [1, 2], [4, 2], [1, 3], [0, 0], [2, 3], [5, 1], [5, 3], [7, 2], [1, 0], [7, 1], [0, 2], [6, 0], [1, 1], [4, 1], [6, 3], [6, 2]], [[5, 0], [7, 0], [4, 2], [0, 2], [7, 2], [5, 1], [1, 2], [6, 1], [3, 3], [3, 1], [4, 1], [6, 0], [1, 3], [2, 0], [6, 3], [5, 3], [4, 3], [1, 1], [0, 3]], [[6, 3], [5, 1], [4, 2], [7, 0], [7, 3], [5, 0], [7, 2], [3, 1], [0, 3], [7, 1], [3, 2], [2, 0], [5, 2], [6, 0], [2, 3], [5, 3], [4, 0]], [[5, 1], [2, 2], [0, 3], [2, 1], [4, 3], [5, 3], [1, 3], [2, 0], [0, 0], [5, 0], [3, 0], [1, 1], [7, 0], [6, 3], [4, 2], [0, 2], [5, 2], [3, 1], [7, 3]], [[2, 3], [7, 1], [2, 2], [3, 2], [4, 2], [7, 3], [5, 2], [0, 2], [4, 1], [5, 1], [1, 0], [1, 3], [2, 0], [7, 2], [1, 1], [6, 3], [0, 1]], [[2, 1], [4, 1], [7, 0], [6, 2], [1, 2], [4, 3], [2, 3], [2, 2], [0, 0], [4, 0], [5, 2], [0, 3], [7, 2], [5, 0], [7, 3], [1, 1], [0, 1]], [[7, 3], [7, 0], [0, 0], [6, 3], [4, 2], [1, 2], [3, 1], [5, 2], [1, 0], [4, 1], [0, 3], [7, 2], [4, 3], [3, 0], [0, 2], [1, 1], [0, 1], [1, 3]], [[6, 1], [4, 3], [4, 0], [2, 1], [7, 3], [1, 0], [3, 0], [5, 3], [6, 0], [6, 3], [5, 0], [7, 0], [0, 3], [5, 2], [2, 2], [2, 0], [7, 1]], [[2, 3], [1, 3], [2, 0], [4, 2], [1, 0], [5, 3], [4, 3], [3, 1], [7, 3], [1, 2], [5, 1], [3, 2], [0, 3], [0, 2], [3, 0], [7, 0], [1, 1], [6, 0], [6, 1]], [[4, 3], [6, 2], [3, 0], [2, 3], [3, 1], [0, 0], [5, 1], [7, 1], [3, 3], [0, 3], [2, 1], [1, 3], [1, 1], [0, 2], [5, 3], [4, 1], [2, 2], [7, 2], [1, 2]], [[0, 0], [1, 0], [7, 1], [4, 2], [3, 0], [7, 0], [2, 3], [3, 1], [6, 3], [7, 3], [5, 0], [5, 2], [3, 2], [4, 3], [1, 3], [2, 2], [0, 2]], [[2, 3], [2, 2], [4, 0], [5, 1], [1, 0], [7, 3], [1, 1], [5, 3], [4, 1], [3, 2], [7, 2], [4, 2], [2, 1], [3, 1], [5, 0], [1, 3], [6, 0], [6, 2]], [[6, 0], [5, 2], [4, 0], [0, 3], [4, 3], [7, 1], [0, 2], [6, 1], [3, 2], [7, 2], [1, 0], [3, 1], [0, 0], [2, 3], [3, 0], [2, 1], [5, 0]], [[3, 2], [1, 0], [5, 3], [4, 2], [1, 3], [3, 0], [1, 2], [3, 3], [5, 0], [5, 2], [4, 0], [2, 2], [6, 3], [6, 2], [3, 1], [0, 1], [4, 3]], [[1, 1], [4, 3], [7, 3], [3, 2], [5, 2], [2, 0], [0, 2], [7, 2], [4, 0], [5, 0], [5, 3], [1, 3], [7, 0], [0, 0], [4, 1], [3, 1], [6, 0], [4, 2], [2, 3]], [[7, 3], [1, 1], [4, 2], [0, 1], [5, 2], [4, 3], [1, 0], [7, 1], [5, 1], [1, 2], [3, 3], [7, 2], [5, 3], [6, 3], [0, 2], [0, 3], [1, 3], [4, 1]], [[6, 0], [3, 3], [1, 0], [2, 3], [6, 1], [0, 0], [5, 1], [1, 2], [4, 2], [7, 0], [0, 2], [3, 0], [5, 2], [7, 1], [1, 1], [2, 0], [7, 2]], [[5, 0], [4, 3], [4, 1], [5, 3], [1, 2], [1, 0], [2, 3], [7, 3], [1, 3], [4, 2], [6, 3], [3, 3], [4, 0], [3, 0], [0, 2], [5, 2], [7, 1]], [[2, 3], [5, 3], [1, 0], [2, 2], [3, 1], [2, 0], [7, 0], [7, 3], [1, 1], [7, 2], [5, 0], [4, 2], [5, 2], [3, 0], [6, 1], [6, 0], [0, 2], [4, 0], [1, 2]], [[6, 1], [1, 3], [3, 2], [6, 2], [1, 1], [5, 2], [0, 0], [4, 0], [7, 3], [7, 0], [2, 2], [0, 2], [6, 0], [2, 0], [5, 1], [7, 2], [1, 2]], [[4, 0], [2, 1], [3, 0], [1, 3], [5, 3], [1, 1], [3, 1], [3, 2], [1, 0], [2, 2], [2, 3], [5, 1], [7, 2], [0, 1], [7, 0], [5, 0], [4, 2], [5, 2], [6, 2]], [[1, 3], [1, 0], [0, 2], [7, 1], [7, 0], [5, 0], [2, 1], [6, 2], [4, 2], [6, 0], [1, 1], [4, 1], [3, 2], [4, 0], [0, 0], [7, 3], [5, 1]], [[6, 3], [3, 0], [2, 3], [3, 1], [1, 2], [0, 2], [5, 3], [7, 3], [0, 1], [6, 1], [1, 3], [4, 3], [4, 1], [4, 2], [2, 2], [5, 2], [1, 0]], [[5, 0], [6, 1], [7, 0], [5, 2], [2, 3], [1, 2], [1, 0], [3, 1], [2, 1], [4, 1], [3, 0], [5, 3], [7, 3], [4, 3], [0, 0], [3, 3], [1, 1], [1, 3]], [[4, 2], [4, 0], [5, 2], [3, 2], [5, 3], [5, 0], [4, 3], [0, 1], [2, 3], [1, 0], [3, 3], [7, 1], [2, 2], [6, 0], [6, 2], [6, 1], [5, 1], [7, 0], [0, 0]], [[4, 0], [4, 1], [3, 3], [6, 3], [4, 2], [7, 3], [1, 1], [0, 3], [6, 1], [5, 2], [6, 0], [3, 2], [3, 0], [1, 2], [1, 3], [2, 2], [7, 1], [5, 3], [7, 2]], [[1, 3], [3, 2], [0, 0], [0, 1], [6, 0], [5, 2], [7, 2], [5, 1], [1, 0], [2, 3], [2, 1], [4, 0], [7, 1], [0, 3], [3, 1], [6, 2], [2, 2], [7, 3], [4, 3]], [[1, 0], [7, 3], [7, 0], [0, 3], [6, 3], [1, 2], [5, 3], [2, 0], [0, 0], [1, 1], [6, 2], [5, 2], [4, 0], [4, 3], [3, 2], [2, 1], [2, 2], [3, 0]], [[4, 0], [0, 2], [6, 0], [5, 3], [5, 2], [6, 2], [0, 1], [3, 0], [2, 1], [4, 3], [1, 0], [2, 0], [1, 2], [2, 3], [7, 2], [7, 0], [4, 2], [3, 2]], [[2, 2], [4, 0], [4, 3], [0, 1], [3, 0], [5, 3], [7, 0], [1, 3], [6, 3], [1, 1], [0, 2], [4, 1], [6, 0], [7, 1], [7, 3], [6, 1], [3, 1]], [[3, 1], [7, 1], [1, 3], [5, 3], [0, 3], [7, 3], [4, 2], [2, 2], [1, 1], [6, 0], [5, 2], [2, 0], [1, 2], [4, 3], [7, 0], [6, 2], [5, 0], [0, 1], [6, 3]], [[3, 3], [7, 3], [0, 1], [0, 2], [2, 1], [5, 2], [4, 0], [7, 0], [6, 1], [6, 3], [6, 0], [4, 3], [2, 3], [1, 2], [1, 1], [0, 3], [2, 2]], [[1, 3], [6, 1], [5, 0], [3, 0], [5, 2], [1, 0], [1, 2], [4, 2], [0, 1], [1, 1], [6, 0], [7, 3], [7, 2], [3, 1], [0, 3], [2, 1], [2, 2]], [[5, 2], [0, 2], [1, 3], [3, 0], [4, 1], [7, 0], [0, 1], [2, 2], [7, 3], [5, 1], [3, 3], [2, 3], [0, 3], [3, 1], [6, 1], [3, 2], [5, 0], [2, 1], [4, 3]], [[4, 0], [1, 0], [7, 2], [6, 1], [2, 1], [7, 0], [0, 1], [3, 0], [3, 3], [5, 0], [1, 3], [3, 2], [2, 3], [2, 0], [7, 3], [0, 0], [0, 2], [6, 3]], [[6, 1], [0, 1], [7, 3], [2, 3], [0, 0], [1, 1], [6, 0], [1, 0], [3, 0], [4, 1], [3, 3], [4, 3], [1, 2], [1, 3], [2, 1], [2, 2], [5, 1], [6, 2]]]
];

var blackBoxesModified = [
    [[[3, 0], [4, 0], [4, 1], [3, 2], [5, 2], [0, 3], [3, 3], [4, 3], [4, 2], [7, 1], [7, 2], [1, 2], [7, 3], [6, 2], [5, 3], [0, 2], [6, 1]], [[1, 0], [3, 3], [4, 2], [4, 1], [2, 0], [2, 1], [2, 3], [5, 0], [4, 3], [4, 0], [7, 0], [6, 2], [0, 2], [1, 2], [1, 3], [7, 1], [5, 3]], [[6, 0], [3, 0], [3, 2], [2, 3], [6, 3], [6, 2], [7, 2], [5, 0], [2, 1], [3, 1], [7, 3], [7, 0], [1, 2], [3, 3], [5, 3], [2, 2], [1, 0]], [[6, 1], [3, 0], [7, 3], [7, 2], [5, 3], [3, 2], [0, 0], [2, 3], [0, 2], [1, 2], [5, 1], [0, 1], [3, 3], [6, 0], [2, 2], [5, 2], [1, 3], [1, 1], [2, 1]], [[4, 3], [2, 0], [7, 2], [6, 0], [0, 0], [3, 3], [7, 0], [5, 2], [6, 2], [5, 3], [4, 2], [6, 1], [6, 3], [4, 1], [1, 2], [1, 1], [7, 3], [4, 0], [5, 1]], [[3, 1], [1, 0], [2, 1], [6, 1], [5, 2], [5, 0], [2, 0], [3, 2], [7, 2], [1, 2], [4, 0], [4, 1], [7, 0], [3, 3], [4, 2], [3, 0], [4, 3], [0, 3], [5, 1]], [[1, 2], [5, 0], [4, 3], [1, 0], [2, 2], [0, 2], [5, 2], [3, 0], [3, 1], [2, 0], [4, 0], [6, 1], [6, 3], [0, 3], [3, 2], [4, 2], [2, 3]], [[7, 2], [5, 1], [0, 2], [6, 1], [7, 1], [0, 0], [2, 3], [3, 0], [3, 2], [5, 2], [0, 1], [5, 3], [7, 0], [6, 0], [2, 1], [6, 3], [5, 0], [1, 0]], [[3, 0], [7, 0], [3, 1], [1, 1], [4, 3], [6, 0], [6, 1], [2, 0], [7, 2], [5, 2], [6, 2], [7, 1], [2, 1], [1, 0], [5, 1], [3, 2], [6, 3]], [[3, 1], [5, 2], [7, 0], [0, 2], [7, 1], [0, 1], [6, 3], [5, 0], [3, 0], [6, 0], [6, 2], [0, 0], [4, 1], [2, 3], [6, 1], [3, 2], [7, 2], [7, 3], [1, 1]], [[4, 0], [6, 2], [4, 2], [5, 2], [7, 3], [6, 1], [0, 2], [7, 0], [1, 0], [6, 0], [1, 3], [5, 1], [7, 2], [4, 1], [1, 2], [0, 3], [3, 0], [4, 3], [2, 1]], [[0, 0], [1, 3], [3, 3], [0, 3], [5, 2], [3, 0], [6, 2], [2, 2], [5, 0], [7, 3], [5, 1], [1, 2], [3, 2], [4, 0], [1, 0], [4, 3], [0, 1], [6, 3]], [[7, 3], [1, 0], [3, 1], [2, 1], [7, 2], [7, 1], [1, 1], [4, 3], [4, 2], [0, 3], [2, 2], [4, 1], [6, 1], [0, 0], [3, 3], [3, 0], [1, 2], [0, 2], [3, 2]], [[1, 2], [2, 3], [5, 0], [5, 2], [3, 1], [4, 0], [1, 1], [4, 2], [5, 1], [3, 2], [2, 1], [4, 3], [1, 3], [0, 1], [3, 3], [2, 2], [7, 3], [6, 2], [0, 2]], [[6, 0], [3, 0], [0, 3], [4, 1], [2, 2], [5, 0], [0, 1], [1, 2], [3, 1], [3, 3], [1, 1], [0, 2], [0, 0], [6, 2], [5, 1], [2, 0], [7, 1], [7, 2]], [[3, 1], [6, 0], [7, 2], [7, 1], [2, 1], [4, 2], [7, 0], [4, 0], [7, 3], [0, 0], [2, 3], [1, 0], [3, 0], [5, 3], [1, 1], [2, 2], [6, 1], [2, 0]], [[1, 2], [1, 0], [6, 1], [0, 2], [3, 3], [2, 1], [6, 3], [2, 0], [4, 1], [1, 3], [4, 0], [5, 2], [7, 3], [3, 1], [4, 3], [3, 0], [1, 1], [7, 0]], [[0, 2], [5, 0], [2, 3], [2, 2], [0, 3], [6, 3], [4, 2], [3, 0], [4, 3], [6, 2], [2, 1], [2, 0], [4, 1], [1, 3], [5, 3], [7, 0], [3, 2]], [[2, 0], [6, 3], [5, 2], [5, 3], [3, 3], [3, 1], [7, 0], [2, 1], [3, 0], [7, 3], [0, 0], [5, 1], [3, 2], [2, 2], [6, 0], [7, 1], [0, 3]], [[5, 2], [3, 2], [5, 0], [3, 0], [7, 1], [6, 1], [3, 1], [7, 3], [5, 1], [2, 2], [4, 3], [4, 1], [1, 3], [6, 0], [5, 3], [7, 0], [0, 1], [0, 3], [0, 2]], [[3, 1], [5, 3], [2, 1], [6, 1], [3, 0], [1, 3], [4, 3], [1, 0], [2, 0], [5, 0], [7, 3], [6, 0], [7, 0], [0, 2], [0, 1], [4, 0], [6, 3], [0, 3], [2, 2]], [[3, 0], [7, 1], [1, 3], [4, 1], [7, 2], [5, 0], [6, 3], [3, 1], [6, 0], [6, 2], [2, 0], [0, 3], [2, 3], [6, 1], [0, 0], [5, 2], [0, 1], [1, 1], [3, 2]], [[7, 0], [6, 2], [4, 1], [0, 1], [4, 0], [1, 0], [3, 2], [6, 0], [2, 0], [6, 1], [2, 1], [3, 3], [7, 2], [2, 2], [3, 1], [5, 1], [4, 2]], [[1, 0], [0, 0], [6, 2], [2, 0], [5, 1], [4, 0], [5, 3], [4, 1], [3, 0], [2, 2], [6, 0], [7, 1], [1, 3], [7, 0], [0, 3], [6, 1], [0, 2], [3, 3], [1, 1]], [[6, 1], [1, 2], [3, 2], [2, 2], [2, 0], [5, 1], [0, 3], [7, 2], [0, 0], [3, 3], [3, 0], [4, 0], [4, 3], [6, 0], [3, 1], [4, 1], [1, 0], [7, 1], [7, 3]], [[4, 1], [0, 3], [6, 2], [7, 1], [3, 2], [1, 1], [2, 2], [5, 2], [1, 3], [4, 3], [5, 1], [5, 0], [4, 0], [3, 0], [2, 0], [6, 1], [0, 2], [3, 3], [6, 0]], [[4, 3], [3, 2], [5, 0], [6, 2], [0, 2], [5, 2], [2, 0], [0, 0], [6, 1], [5, 1], [0, 1], [2, 1], [7, 1], [6, 0], [7, 0], [1, 2], [3, 3], [4, 1]], [[5, 3], [1, 2], [6, 0], [4, 2], [6, 2], [0, 0], [2, 3], [5, 1], [6, 3], [7, 2], [1, 3], [2, 2], [1, 0], [7, 1], [5, 2], [4, 3], [7, 3], [2, 0]], [[2, 0], [3, 1], [6, 0], [6, 1], [0, 2], [5, 1], [1, 3], [4, 2], [6, 3], [5, 3], [7, 0], [1, 1], [7, 2], [5, 0], [4, 3], [3, 3], [2, 3], [3, 0], [2, 2]], [[3, 2], [5, 1], [0, 3], [7, 0], [2, 0], [7, 1], [7, 2], [5, 2], [6, 0], [4, 0], [7, 3], [6, 3], [4, 2], [5, 0], [1, 3], [6, 1], [1, 0]], [[6, 3], [7, 3], [4, 2], [0, 3], [3, 1], [5, 1], [0, 2], [2, 2], [7, 0], [2, 0], [2, 1], [4, 3], [5, 3], [1, 1], [0, 0], [6, 0], [7, 1], [3, 3], [4, 1]], [[4, 1], [2, 3], [2, 0], [1, 0], [5, 1], [7, 2], [0, 2], [5, 2], [1, 1], [7, 1], [2, 2], [0, 1], [4, 2], [6, 3], [6, 0], [6, 2], [4, 0]], [[0, 0], [4, 0], [1, 2], [1, 1], [2, 2], [7, 2], [7, 0], [6, 2], [5, 2], [2, 3], [2, 1], [4, 3], [7, 3], [2, 0], [6, 3], [3, 3], [3, 2]], [[4, 1], [3, 1], [1, 0], [3, 0], [0, 1], [7, 3], [0, 2], [6, 3], [5, 2], [7, 2], [7, 0], [1, 2], [0, 3], [1, 3], [4, 2], [3, 3], [6, 1], [6, 0]], [[7, 0], [4, 3], [3, 0], [6, 0], [5, 2], [5, 0], [4, 0], [2, 0], [2, 2], [6, 1], [1, 0], [7, 1], [2, 1], [3, 3], [2, 3], [3, 2], [6, 2]], [[3, 0], [3, 2], [6, 1], [2, 3], [5, 3], [1, 3], [4, 3], [1, 1], [0, 2], [1, 0], [4, 2], [0, 3], [7, 0], [1, 2], [5, 1], [3, 3], [4, 0], [6, 2], [6, 3]], [[6, 2], [0, 0], [0, 3], [4, 1], [3, 0], [1, 1], [5, 3], [0, 2], [4, 3], [7, 1], [1, 3], [2, 3], [3, 3], [7, 2], [2, 1], [5, 0], [7, 3], [3, 2], [4, 0]], [[3, 1], [4, 3], [7, 3], [3, 0], [0, 0], [2, 2], [4, 2], [6, 3], [5, 0], [3, 2], [7, 0], [0, 2], [1, 0], [2, 3], [5, 3], [5, 1], [0, 3]], [[4, 1], [4, 0], [2, 2], [7, 2], [2, 3], [1, 1], [3, 1], [5, 3], [1, 0], [4, 2], [5, 0], [1, 3], [5, 1], [3, 2], [7, 0], [6, 1], [7, 1], [6, 3]], [[0, 0], [6, 1], [7, 2], [5, 2], [2, 3], [3, 2], [2, 1], [4, 3], [5, 0], [3, 0], [7, 1], [1, 0], [6, 0], [5, 1], [2, 2], [6, 2], [1, 3]], [[1, 3], [0, 1], [6, 2], [3, 0], [5, 2], [4, 0], [2, 2], [4, 2], [1, 0], [4, 3], [1, 2], [3, 3], [3, 2], [1, 1], [2, 0], [0, 3], [5, 1]], [[5, 0], [5, 2], [4, 3], [7, 0], [2, 0], [1, 3], [3, 2], [1, 1], [5, 3], [0, 2], [0, 0], [6, 0], [4, 2], [7, 2], [4, 1], [3, 1], [6, 2], [2, 1], [1, 2]], [[7, 3], [6, 3], [7, 2], [0, 3], [7, 1], [1, 3], [3, 3], [1, 2], [0, 1], [5, 3], [4, 2], [4, 3], [5, 1], [1, 1], [6, 2], [3, 2], [2, 0], [7, 0]], [[1, 1], [7, 2], [5, 2], [6, 0], [7, 0], [3, 3], [4, 2], [0, 2], [2, 0], [7, 1], [3, 0], [2, 3], [0, 0], [6, 1], [2, 1], [6, 2], [1, 3]], [[1, 0], [1, 3], [7, 3], [4, 0], [5, 2], [5, 0], [3, 3], [2, 3], [4, 1], [7, 1], [3, 0], [5, 3], [4, 2], [4, 3], [3, 2], [0, 0], [0, 3]], [[2, 0], [1, 2], [5, 2], [0, 2], [4, 2], [4, 0], [3, 1], [7, 0], [1, 0], [7, 3], [3, 0], [5, 3], [5, 0], [6, 0], [1, 1], [6, 1], [0, 1], [1, 3], [2, 1]], [[6, 0], [1, 1], [7, 3], [6, 1], [2, 2], [5, 2], [1, 3], [7, 0], [4, 0], [5, 1], [0, 0], [6, 2], [7, 2], [0, 1], [4, 2], [5, 0], [5, 3]], [[0, 1], [2, 3], [5, 2], [5, 0], [5, 1], [7, 0], [4, 0], [3, 2], [3, 0], [4, 2], [6, 2], [1, 1], [2, 1], [2, 2], [1, 0], [7, 3], [4, 3], [6, 3], [4, 1]], [[1, 3], [6, 2], [2, 1], [4, 2], [7, 0], [1, 0], [4, 1], [6, 0], [1, 1], [0, 2], [3, 2], [7, 3], [7, 1], [5, 0], [2, 3], [6, 1], [5, 3]], [[4, 3], [1, 0], [7, 3], [4, 2], [5, 2], [0, 1], [1, 3], [3, 0], [6, 3], [2, 2], [0, 2], [3, 1], [6, 1], [1, 2], [7, 2], [5, 1], [2, 0]], [[5, 0], [5, 2], [3, 1], [7, 3], [1, 2], [1, 3], [5, 3], [3, 3], [2, 3], [4, 3], [1, 0], [6, 1], [7, 0], [4, 1], [0, 0], [0, 1], [7, 1], [5, 1]], [[6, 0], [0, 0], [4, 0], [2, 2], [7, 0], [5, 3], [0, 1], [7, 1], [4, 3], [2, 3], [6, 1], [6, 2], [3, 3], [3, 2], [5, 0], [5, 1], [0, 3], [1, 2], [2, 0]], [[1, 2], [6, 1], [3, 0], [4, 2], [0, 3], [4, 0], [1, 1], [3, 3], [4, 1], [6, 3], [7, 3], [7, 2], [5, 2], [6, 0], [2, 2], [1, 3], [0, 0], [5, 0], [7, 0]], [[5, 1], [4, 3], [0, 3], [0, 1], [5, 2], [1, 0], [7, 2], [6, 2], [2, 3], [3, 2], [7, 1], [1, 3], [2, 1], [2, 2], [3, 1], [6, 0], [6, 1], [1, 1], [5, 0]], [[2, 0], [2, 2], [4, 3], [3, 2], [7, 3], [5, 2], [1, 1], [5, 3], [1, 0], [2, 1], [6, 2], [0, 3], [1, 2], [0, 0], [5, 0], [6, 0], [7, 2], [6, 1]], [[3, 0], [2, 0], [7, 0], [7, 2], [5, 3], [0, 2], [6, 2], [4, 2], [4, 0], [1, 2], [3, 2], [5, 2], [4, 3], [2, 1], [4, 1], [5, 1], [3, 3], [0, 3]], [[5, 3], [2, 2], [7, 0], [7, 1], [0, 2], [6, 0], [1, 1], [6, 3], [3, 1], [7, 3], [4, 1], [0, 1], [1, 3], [3, 0], [7, 2], [5, 2], [2, 3]], [[6, 3], [2, 0], [1, 3], [7, 1], [0, 1], [5, 3], [1, 1], [1, 2], [5, 2], [7, 3], [4, 2], [6, 0], [2, 2], [6, 2], [0, 3], [1, 0], [4, 1], [5, 1], [0, 2]], [[0, 2], [4, 0], [2, 2], [3, 3], [0, 1], [1, 1], [2, 1], [7, 3], [2, 3], [6, 3], [0, 3], [6, 1], [6, 0], [7, 0], [3, 0], [2, 0], [5, 3]], [[1, 2], [2, 1], [0, 1], [3, 1], [0, 3], [2, 2], [5, 0], [4, 2], [5, 2], [6, 0], [7, 3], [1, 1], [3, 0], [7, 1], [0, 0], [6, 2], [4, 1]], [[5, 0], [0, 3], [2, 3], [4, 3], [3, 0], [5, 2], [4, 1], [2, 2], [1, 3], [6, 1], [3, 2], [7, 0], [3, 1], [7, 3], [0, 2], [6, 3], [1, 2], [6, 2], [1, 1]], [[0, 2], [3, 0], [3, 3], [4, 0], [7, 0], [6, 1], [2, 0], [1, 3], [0, 0], [5, 0], [2, 3], [6, 3], [7, 3], [3, 2], [4, 2], [5, 1], [5, 3], [1, 2]], [[6, 2], [2, 1], [2, 3], [2, 2], [1, 1], [3, 0], [1, 2], [7, 3], [1, 3], [1, 0], [5, 1], [3, 3], [6, 0], [4, 1], [3, 2], [4, 0], [7, 0], [0, 2]]]
];

var psSymmetric = [
    [0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1]
];

var selections_symm = [
    [[[0, 0], [1, 3], [3, 0]], [[2, 1], [3, 0], [0, 3], [1, 3]], [[3, 1], [2, 0], [3, 3], [2, 2]], [[0, 0], [0, 1], [1, 3]], [[3, 1], [1, 1], [1, 0]], [[2, 1], [0, 0], [1, 3], [1, 1]], [[0, 3], [1, 1], [3, 3], [0, 2]], [[1, 0], [0, 1], [3, 3]], [[3, 3], [0, 1], [1, 3], [2, 3], [3, 1]], [[0, 2], [1, 3], [2, 1], [3, 1], [0, 1]], [[1, 3], [3, 3]], [[1, 3], [0, 3], [3, 0], [3, 3]], [[2, 2], [3, 2], [1, 2], [1, 0]], [[2, 1], [0, 2], [0, 3]], [[1, 2], [0, 2]], [[3, 2], [1, 3], [2, 3], [3, 3], [3, 1]], [[2, 0], [3, 0], [1, 0]], [[0, 1], [2, 3]]]
];


var trial_type_updating = [
    "['update', 'immediate', 'update', 'update', 'update', 'update', 'update', 'immediate', 'update', 'update', 'immediate', 'update', 'update', 'update', 'immediate', 'update', 'update', 'update', 'update', 'update', 'update', 'update', 'immediate', 'update', 'immediate', 'update', 'update']",
    "['update', 'immediate', 'update', 'update', 'update', 'immediate', 'immediate', 'update', 'immediate', 'update', 'update', 'update', 'update', 'update', 'update', 'update', 'update', 'update', 'update', 'immediate', 'immediate', 'update', 'update', 'update', 'update', 'update', 'update']"
]

var initial_set_updating = [
    "[[3, 6, 9, 4],[8, 6, 3, 2],[0, 9, 6, 4],[1, 6, 5, 8],[9, 5, 4, 1],[6, 3, 8, 4],[9, 4, 7, 1],[4, 3, 8, 6],[5, 8, 1, 4],[4, 1, 9, 8],[8, 2, 7, 1],[7, 6, 5, 0],[2, 6, 3, 7],[9, 5, 8, 1],[2, 9, 8, 7],[1, 3, 7, 0],[9, 6, 5, 8],[0, 8, 2, 4],[3, 2, 5, 9],[2, 0, 5, 8],[7, 0, 6, 8]]",
    "[[5, 8, 0, 9],[0, 9, 5, 8],[9, 2, 4, 0],[6, 7, 8, 5],[6, 8, 4, 9],[6, 5, 7, 2],[0, 4, 6, 9],[3, 5, 0, 9],[1, 7, 8, 5],[2, 9, 8, 3],[9, 4, 8, 3],[4, 2, 5, 9],[7, 8, 3, 5],[0, 9, 7, 4],[2, 9, 3, 0],[1, 6, 0, 5],[3, 8, 6, 5],[0, 7, 3, 5],[2, 5, 4, 8],[8, 5, 2, 7],[4, 7, 0, 5]]"
]

var immediate_set_updating = [
    "[[1, 7, 3, 6],[9, 2, 3, 0],[0, 9, 4, 3],[2, 5, 1, 3],[0, 8, 6, 7],[5, 3, 9, 2]]",
    "[[0, 9, 2, 1],[4, 2, 3, 6],[5, 0, 4, 2],[8, 9, 6, 0],[8, 2, 1, 0],[0, 9, 1, 7]]"
]

var locations_update_updating = [
    "[[3, 3, 1, 3, 2, 1, 3],[2, 0, 1, 2, 3, 0, 2],[2, 0, 1, 1, 1, 3, 0],[2, 3, 1, 0, 0, 2, 2],[0, 1, 1, 1, 2, 1, 2],[0, 3, 1, 3, 3, 2, 0],[3, 1, 2, 3, 0, 0, 2],[3, 1, 3, 2, 0, 0, 1],[3, 3, 1, 2, 3, 3, 0],[3, 2, 2, 2, 1, 2, 1],[1, 3, 1, 2, 1, 0, 1],[2, 3, 0, 0, 1, 2, 1],[3, 3, 0, 1, 0, 2, 2],[0, 3, 2, 3, 0, 0, 0],[0, 3, 0, 3, 1, 3, 1],[1, 2, 3, 2, 3, 0, 3],[3, 2, 1, 2, 3, 1, 3],[1, 2, 3, 3, 3, 2, 0],[1, 2, 0, 2, 2, 0, 0],[1, 0, 1, 0, 2, 1, 3],[1, 0, 1, 0, 2, 3, 1]]",
    "[[2, 0, 3, 0, 2, 1, 1],[2, 2, 1, 3, 1, 1, 0],[0, 2, 3, 0, 2, 1, 0],[1, 3, 0, 2, 2, 2, 2],[0, 0, 2, 1, 2, 1, 2],[2, 1, 1, 0, 0, 2, 1],[0, 2, 3, 2, 0, 0, 3],[0, 1, 1, 0, 0, 1, 2],[3, 2, 2, 2, 2, 1, 3],[3, 3, 0, 1, 3, 1, 1],[2, 3, 0, 2, 0, 3, 2],[1, 0, 1, 0, 2, 0, 3],[3, 0, 2, 1, 0, 1, 1],[2, 1, 3, 3, 2, 3, 0],[0, 2, 3, 2, 0, 0, 3],[2, 2, 1, 2, 1, 0, 3],[0, 2, 1, 2, 2, 1, 2],[3, 1, 0, 3, 0, 3, 3],[1, 1, 3, 3, 1, 1, 2],[1, 0, 1, 3, 1, 2, 1],[1, 1, 2, 3, 2, 1, 2]]"
]

var items_replace_updating = [
    "[[3, 6, 5, 4, 4, 1, 1],[3, 0, 9, 9, 1, 6, 7],[8, 2, 0, 1, 6, 1, 3],[4, 6, 2, 2, 0, 8, 6],[0, 1, 9, 5, 4, 2, 6],[1, 3, 9, 8, 0, 4, 9],[2, 8, 6, 8, 3, 9, 7],[0, 2, 5, 3, 5, 2, 3],[5, 0, 5, 8, 8, 8, 5],[6, 6, 7, 9, 5, 8, 1],[4, 0, 7, 0, 5, 1, 7],[9, 7, 3, 6, 2, 3, 3],[5, 2, 4, 9, 2, 4, 0],[8, 2, 3, 9, 3, 5, 4],[4, 4, 4, 9, 4, 9, 0],[6, 9, 8, 4, 4, 4, 7],[8, 2, 2, 4, 5, 4, 4],[7, 9, 7, 1, 3, 2, 3],[0, 5, 2, 6, 7, 7, 2],[6, 3, 3, 9, 8, 7, 1],[8, 5, 1, 5, 7, 3, 6]]",
    "[[0, 1, 0, 1, 8, 9, 0],[5, 7, 8, 0, 8, 9, 9],[1, 5, 4, 0, 1, 6, 7],[3, 9, 3, 1, 5, 4, 8],[9, 4, 1, 3, 6, 8, 9],[3, 0, 1, 4, 9, 8, 3],[4, 8, 5, 4, 0, 8, 5],[5, 8, 0, 0, 4, 3, 9],[6, 8, 2, 8, 0, 9, 5],[2, 3, 6, 5, 4, 1, 9],[5, 5, 3, 4, 0, 0, 2],[0, 4, 7, 3, 7, 9, 4],[2, 4, 2, 9, 8, 7, 5],[8, 5, 8, 1, 2, 5, 7],[5, 2, 2, 1, 0, 2, 8],[4, 6, 3, 9, 1, 4, 3],[6, 0, 0, 9, 9, 2, 2],[2, 5, 0, 0, 5, 9, 8],[5, 8, 7, 8, 8, 8, 5],[5, 3, 8, 2, 7, 4, 8],[5, 3, 5, 1, 4, 6, 9]]"
]

var final_set_updating = [
    "[[3, 1, 4, 1],[6, 9, 7, 1],[3, 6, 8, 1],[0, 2, 6, 6],[0, 2, 6, 1],[9, 9, 4, 0],[9, 8, 7, 8],[2, 3, 3, 5],[5, 5, 8, 8],[4, 1, 8, 6],[1, 7, 0, 0],[6, 3, 3, 7],[2, 9, 0, 2],[4, 5, 3, 9],[4, 0, 8, 9],[4, 6, 4, 7],[9, 4, 4, 4],[3, 7, 2, 3],[2, 0, 7, 9],[9, 7, 8, 1],[5, 6, 7, 3]]",
    "[[1, 0, 8, 0],[9, 9, 7, 0],[7, 6, 1, 4],[3, 3, 8, 9],[4, 8, 9, 9],[9, 3, 8, 2],[8, 4, 4, 5],[4, 3, 9, 9],[1, 9, 0, 5],[6, 9, 8, 4],[0, 4, 2, 0],[9, 7, 7, 4],[8, 5, 2, 2],[7, 5, 2, 5],[2, 9, 1, 8],[4, 1, 9, 3],[6, 2, 2, 5],[5, 5, 3, 8],[2, 8, 5, 8],[3, 8, 4, 2],[4, 6, 9, 1]]"
]