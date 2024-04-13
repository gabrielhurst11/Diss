function openInputBox(title) {
    if (title == 'Assumption'){
        $('#inputModalLabel').text('Enter Proposition for ' + title);
        $('#propositionInput').val('');
        $('#inputModal').modal('show');
    }
    else if (title == 'Conjunction Elimination (L)' || title == 'Conjunction Elimination (R)' || title === 'Apply DML'){
        $('#conjModalLabel').text('Choose Proposition for ' + title);
        $('#propositionChoice').val('');
        populateDropdown();
        var button = document.querySelector('#conjModal .modal-footer button.btn-primary');
        if (title === "Conjunction Elimination (L)") {
            button.setAttribute("onclick", "sendResolution('r 1')");
        } else if (title === "Conjunction Elimination (R)") {
            button.setAttribute("onclick", "sendResolution('r 2')");
        } else if (title === 'Apply DML'){
            button.setAttribute("onclick", "sendResolution('r 7')");
        }
        $('#conjModal').modal('show');
    }
    else if (title == 'Conjunction Introduction' || title == 'Implication Introduction' || title === 'Modus Ponens' || title === 'Modus Tolens'){
        $('#intModalLabel').text('Enter Propositions for ' + title);
        $('#propositionChoice').val('');
        populateDropdown2();
        var button = document.querySelector('#intModal .modal-footer button.btn-primary');
        if (title === "Conjunction Introduction") {
            button.setAttribute("onclick", "sendIntroduction('r 3')");
        }
        if (title === "Implication Introduction"){
            button.setAttribute("onclick", "sendIntroduction('r 4')")
        }
        if (title === "Modus Ponens"){
            button.setAttribute("onclick", "sendIntroduction('r 8')")
        }
        if (title === "Modus Tolens"){
            button.setAttribute("onclick", "sendIntroduction('r 9')")
        }
        $('#intModal').modal('show');
    }
    else if (title === 'Disjunction Introduction (L)' || title === "Disjunction Introduction (R)"){
        $('#disjModalLabel').text('Choose Proposition for ' + title);
        $('#conjInput').val('');
        populateDropdown3();
        var button = document.querySelector('#disjModal .modal-footer button.btn-primary');
        if (title === "Disjunction Introduction (L)") {
            button.setAttribute("onclick", "sendIntroduction('r 5')");
        }
        else if (title === "Disjunction Introduction (R)") {
            button.setAttribute("onclick","sendIntroduction('r 6')" );
        }
        $('#disjModal').modal('show');

    }
  }

function populateDropdown() {
    var dropdown = document.getElementById("stepDropdown");
    dropdown.innerHTML = ""; // Clear previous options

    for (var i = 0; i < steps.length; i++) {
        var option = document.createElement("option");
        option.text = "Step " + (i + 1) + ": " + steps[i];
        option.value = i;
        dropdown.appendChild(option);
    }
}
function populateDropdown3() {
    var dropdown = document.getElementById("stepDropdown3");
    dropdown.innerHTML = ""; // Clear previous options

    for (var i = 0; i < steps.length; i++) {
        var option = document.createElement("option");
        option.text = "Step " + (i + 1) + ": " + steps[i];
        option.value = i;
        dropdown.appendChild(option);
    }
}
function populateDropdown2() {
    var dropdown1 = document.getElementById("stepDropdown1");
    var dropdown2 = document.getElementById("stepDropdown2");
    dropdown1.innerHTML = ""; // Clear previous options
    dropdown2.innerHTML = ""; // Clear previous options
    for (var i = 0; i < steps.length; i++) {
        var option1 = document.createElement("option");
        option1.text = "Step " + (i + 1) + ": " + steps[i];
        option1.value = i;
        dropdown1.appendChild(option1);

        var option2 = document.createElement("option");
        option2.text = "Step " + (i + 1) + ": " + steps[i];
        option2.value = i;
        dropdown2.appendChild(option2);
    }
}