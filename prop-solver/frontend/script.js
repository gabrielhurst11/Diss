// Connect to the WebSocket server
const socket = new WebSocket('ws://localhost:8080');
var currentRequest = "Table";
let currentProposition;

// Variable to keep track of the steps
let steps = [];

function clearSteps(){
    steps = [];
    displaySteps();
}
function createSteps(){
    clearSteps();
    const expression = document.getElementById('expressionInput').value.trim();

    // Save the proposition as a step
    steps.push(expression);

    // Display steps as numbered list
    displaySteps();
}

function addAssum(){
    const expression = document.getElementById('propositionInput').value.trim();

    steps.push(expression);

    displaySteps();
}

function addStep(step){
    steps.push(step);
    displaySteps();
}

function displaySteps(){
    const outputDiv = document.getElementById('output');
    outputDiv.innerHTML = '';

    const stepsList = document.createElement('ol');

    // Loop through steps and create list items
    steps.forEach((step, index) => {
        const stepItem = document.createElement('li');
        stepItem.textContent = `${step}`;
        stepsList.appendChild(stepItem);
    });

    outputDiv.appendChild(stepsList);
}

function appendToInput(text) {
    var input = document.getElementById("expressionInput");
    input.value += text;
}

function clearInput() {
    document.getElementById("expressionInput").value = "";
}

// Function to update the UI with the received data
function updateUI(data) {
    const outputDiv = document.getElementById("output");
    if (currentRequest == "Table"){
        outputDiv.innerHTML = "<h2>Truth Table</h2>";
        outputDiv.innerHTML += createTableFromData(data);
    }
    else if (currentRequest == "Resolution"){
        addStep(data);
    }
}

// Function to display the result on the page
function displayResult(result) {
    const outputDiv = document.getElementById('output');
    outputDiv.innerHTML = '<h5>Result:</h5><p>' + result + '</p>';
}

// Function to create an HTML table from the received data
function createTableFromData(data) {
    const rows = data.split('\n').filter(Boolean);
    let tableHTML = `<table class="table table-bordered table-striped"><thead><tr>`;

    // Extract headers from the first row
    const headers = rows[0].split(' ');

    // Create header cells
    headers.forEach(header => {
        if (header !== "Result") {
            tableHTML += `<th scope="col">${header}</th>`;
        } else {
            tableHTML += `<th colspan="${headers.length - 1}" scope="col">${header}</th>`;
        }
    });

    // Close header row
    tableHTML += `</tr></thead><tbody>`;

    // Create data rows
    for (let i = 1; i < rows.length; i++) {
        const cells = rows[i].split(' ');
        tableHTML += `<tr>`;
        cells.forEach(cell => {
            tableHTML += `<td>${cell}</td>`;
        });
        tableHTML += `</tr>`;
    }

    // Close table body and table
    tableHTML += `</tbody></table>`;

    return tableHTML;
}

// WebSocket event handlers
socket.onopen = function() {
    console.log("WebSocket connection established.");
};

socket.onmessage = function(event) {
    const data = event.data;
    updateUI(data);
};

function sendProposition(requestType) {
    const expression = document.getElementById('expressionInput').value;
    if (requestType === 't'){
        currentRequest = "Table";
    }
    else{
        currentRequest = "Resolution";
    }    
    // Save the proposition to the currentProposition variable
    currentProposition = expression;
    
    // Display the proposition on the page
    document.getElementById('output').innerText = currentProposition;
    
    // Send the proposition to the server
    const parsedExpression = parseExpression(expression, requestType);
    console.log(parsedExpression);
    socket.send(parsedExpression);
}
function sendResolution(requestType) {
    const expressionIndex = document.getElementById('stepDropdown').value;
    var expression = steps[expressionIndex];
    currentRequest = "Resolution";   
    // Save the proposition to the currentProposition variable
    currentProposition = expression;
    
    // Display the proposition on the page
    document.getElementById('output').innerText = currentProposition;
    
    // Send the proposition to the server
    const parsedExpression = parseExpression(expression, requestType);
    console.log(parsedExpression);
    socket.send(parsedExpression);
}
function sendIntroduction(requestType) {
    const expressionIndex1 = document.getElementById('stepDropdown1').value;
    var expression1 = steps[expressionIndex1];
    const expressionIndex2 = document.getElementById('stepDropdown2').value;
    var expression2 = steps[expressionIndex2];
    currentRequest = "Resolution";   
    // Save the propositions to the currentProposition variable
    currentProposition = expression1 + " , " + expression2;
    
    // Display the proposition on the page
    document.getElementById('output').innerText = currentProposition;
    
    // Send the proposition to the server
    const parsedExpression = parseExpression(currentProposition, requestType);
    console.log(parsedExpression);
    socket.send(parsedExpression);
}

// Function to parse the expression and convert it into the desired format
function parseExpression(expression, requestType) {
    console.log(expression);
    // Helper function to remove spaces from both ends of a string
    const trim = (str) => str.trim();

    // Remove leading and trailing whitespace
    expression = trim(expression);

    // If the expression starts with a '(' and ends with a ')',
    // strip the outer parentheses and parse the inner expression
    if (expression.startsWith('(') && expression.endsWith(')')) {
        return parseExpression(expression.slice(1, -1));
    }

    // Split the expression by spaces
    const tokens = expression.split(' ');

    // Map each token to its corresponding representation
    const mappedTokens = tokens.map(token => {
        // If the token is 'AND', 'OR', 'NOT', or 'IMPLY', return the uppercase version
        if (['AND', 'OR', 'NOT', 'IMPLY'].includes(token.toUpperCase())) {
            return token;
        }
        if (token == ','){
            return ','
        }
        // Otherwise, assume it's a variable and return it wrapped with 'Var'
        return `Var '${token}'`;
    });

    // Prepend the string value of requestType to the expression
    mappedTokens.unshift(requestType);

    // Join the mapped tokens with spaces to form the final expression
    return mappedTokens.join(' ');
}

function openInputBox(title) {
    if (title == 'Assumption'){
        $('#inputModalLabel').text('Enter Proposition for ' + title);
        $('#propositionInput').val('');
        $('#inputModal').modal('show');
    }
    else if (title == 'Conjunction Elimination (L)' || title == 'Conjunction Elimination (R)'){
        $('#conjModalLabel').text('Choose Proposition for ' + title);
        $('#propositionChoice').val('');
        populateDropdown();
        var button = document.querySelector('#conjModal .modal-footer button.btn-primary');
        if (title === "Conjunction Elimination (L)") {
            button.setAttribute("onclick", "sendResolution('r 1')");
        } else if (title === "Conjunction Elimination (R)") {
            button.setAttribute("onclick", "sendResolution('r 2')");
        }
        $('#conjModal').modal('show');
    }
    else if (title == 'Conjunction Introduction'){
        $('#intModalLabel').text('Enter Propositions for ' + title);
        $('#propositionChoice').val('');
        populateDropdown2();
        var button = document.querySelector('#intModal .modal-footer button.btn-primary');
        if (title === "Conjunction Introduction") {
            button.setAttribute("onclick", "sendIntroduction('r 3')");
        }
        $('#intModal').modal('show');
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


function parseProp(prop){
    let stack = [];
    let queue = [];
    let operators = ['And', 'Or', 'Implies']
    const tokens = prop.split(' ')
    let operator = ""
    let startIndex = -1;
    for (let i = 0; i < tokens.length; i++){
        if (tokens[i] == '('){
            if (startIndex == -1){
                startIndex = i;
            }
            for (let j = i; j < tokens.length; j++){
                if (tokens[j]== '('){
                    stack.push(j);
                    if (stack.length == 1){
                        startIndex = j
                    } 
                }
                if (tokens[j] == ')'){
                    if (stack.length == 0){
                        console.log("Unmatched )")
                        return null;
                    }
                    stack.pop();
                    if (stack.length == 0){
                        
                        let expressionWithinBrackets = tokens.slice(startIndex + 1,j).join(' ');
                        console.log(expressionWithinBrackets);
                        queue.push(parseProp(expressionWithinBrackets));
                        i = j;
                    }
                }
            }
        }else if (operators.includes(tokens[i])){
            operator = tokens[i];
        }else{
            queue.push(tokens[i]);
        }

    }
    let finalProp = "";
    if (queue.length == 1){
        finalProp = queue.shift();
        return finalProp;
    }
    finalProp = operator + " " + queue.shift() + " " + queue.shift();

    return finalProp;
}
    

const inputString = "( ( A And B ) Or ( B And A ) )";
const extractedExpression = parseProp(inputString);
console.log(extractedExpression);

