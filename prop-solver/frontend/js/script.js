// Connect to the WebSocket server
const socket = new WebSocket('ws://localhost:8080');
var currentRequest = "Table";
let currentProposition;

// Variable to keep track of the steps
let steps = [];

// Create list of rules used and what they have used to derive answer
let rules = [];

function clearSteps(){
    rules = [];
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
    addRules("Assum", "");

    displaySteps();
    $('#inputModal').modal('hide');
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
        if (index != 0){
            const rule = rules[index - 1];
            stepItem.textContent = `${step}     (Rule: ${rule})`;
        }
        else{
            stepItem.textContent = `${step}`;
        }
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
    if (currentRequest === "Table"){
        outputDiv.innerHTML = ''; //Clear current Table
        outputDiv.innerHTML += createTableFromData(data);
    }
    else if (currentRequest === "Resolution"){
        addStep(data);
    }
    else if (currentRequest === "SAT"){
        const treeDiv = document.getElementById("tree");
        outputDiv.innerHTML = '';
        treeDiv.innerHTML = '';
        createDPLLTable(data,outputDiv,treeDiv)
    }
}


// Function to create an HTML table from the received data
function createTableFromData(data) {
    const rows = data.split('\n').filter(Boolean);
    let tableHTML = `<div class="table-wrapper"><table class="table table-bordered table-striped"><thead><tr>`;

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

function createDPLLTable(data, outputDiv, treeDiv) {
    const stepsArray = data.split('\n').filter(Boolean);

    // Start creating the table HTML
    let tableHTML = `<div class="table-wrapper"><table class="table table-bordered table-striped"><thead><tr>`;

    for (let i = 0; i < 5 && i < stepsArray.length; i++) {
        const step = stepsArray[i];
        const [stepNumber, description, result] = step.split(': ');

        // Create a table row for each step
        tableHTML += `<tr>`;
        tableHTML += `<td><strong>${stepNumber}</strong></td>`;
        tableHTML += `<td>${description}</td>`;
        tableHTML += `<td>${result}</td>`;
        tableHTML += `</tr>`;
    };

    // Close table body and table
    tableHTML += `</tbody></table></div>`;
    console.log(stepsArray);

    outputDiv.innerHTML += tableHTML;
    const treeString = stepsArray[5];
    // Parse the tree string
    const tree = parseTreeString(treeString);

    // Generate HTML for the tree
    const treeHTML = generateHTML(tree);

    treeDiv.innerHTML = `<ul>${treeHTML}</ul>`;

}

// WebSocket event handlers
socket.onopen = function() {
    console.log("WebSocket connection established.");
};

socket.onmessage = function(event) {
    const data = event.data;
    if (data == "Failed to process request"){
        showError();
    }
    else{
        updateUI(data);
    }
};

function showError(){
    console.log("Backend could not process request")
}

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
    
    addRules(requestType, parseInt(expressionIndex) + 1);
    // Send the proposition to the server
    const parsedExpression = parseExpression(expression, requestType);
    console.log(parsedExpression);
    socket.send(parsedExpression);
    $('#conjModal').modal('hide');
}

function sendIntroduction(requestType) {
    const expressionIndex1 = document.getElementById('stepDropdown1').value;
    var expression1 = steps[expressionIndex1];
    if (requestType === 'r 5' || requestType === 'r 6'){
        const expressionIndex1 = document.getElementById('stepDropdown3').value;
        var expression1 = steps[expressionIndex1];
        var expression2 = document.getElementById('disjInput').value;
        var pointer = parseInt(expressionIndex1) + 1
        addRules(requestType, pointer);
    }
    else{
        const expressionIndex1 = document.getElementById('stepDropdown1').value;
        var expression1 = steps[expressionIndex1];
        const expressionIndex2 = document.getElementById('stepDropdown2').value;
        var expression2 = steps[expressionIndex2];
        var p1 = parseInt(expressionIndex1) + 1;
        var p2 = parseInt(expressionIndex2) + 1;
        var pointer = p1 + ", " + p2; 
        addRules(requestType, pointer);
    }
    currentRequest = "Resolution";   
    // Save the propositions to the currentProposition variable
    currentProposition = expression1 + " , " + expression2;
    currentProp1 = parseExpression(expression1, requestType);
    currentProp2 = parseExpression(expression2, "");
    currentProp3 = currentProp1 + " ," + currentProp2;

    console.log(currentProp3);
    
    socket.send(currentProp3);

    $('#intModal').modal('hide');
    $('#disjModal').modal('hide');
}

function sendSATProp(requestType){
    const expression = document.getElementById('expressionInput').value;
    currentProposition = expression;
    currentRequest = "SAT";
    
    // Send the proposition to the server
    const parsedExpression = parseExpression(expression, requestType);
    console.log(parsedExpression);
    socket.send(parsedExpression);
    
}

function addRules(requestType, index){
    let rule = "";
    if (requestType === 'r 1' || requestType === 'r 2'){
        rule = "conjE,  " + index; 
    }
    else if (requestType === 'r 7'){
        rule = "DeMorgan,  " + index;
    }
    else if (requestType === 'r 5' || requestType === 'r 6'){
        rule = "disjI,  " + index;
    }
    else if (requestType === 'r 8' || requestType === 'r 9'){
        rule = "impE,  " + index;
    }
    else if (requestType === 'r 4'){
        rule = "impI,  " + index;
    }
    else if (requestType === 'r 3'){
        rule = "conjI,  " + index;
    }
    else if (requestType === 'Assum'){
        rule = "Assum";
    }
    rules.push(rule);
    console.log(rules);
}


// Function to parse the expression and convert it into the desired format
function parseExpression(expression, requestType) {
    expression = parseProp(expression);
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
        if (['AND', 'OR', 'NOT', 'IMPLY', 'BIIMPLY'].includes(token.toUpperCase())) {
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

function parseProp(prop){
    console.log(prop);
    let stack = [];
    let queue = [];
    let operators = ['And', 'Or', 'Imply', 'BiImply']
    const tokens = prop.split(' ')
    let operator = ""
    let startIndex = -1;
    let negateNext = false;
    for (let i = 0; i < tokens.length; i++){
        if (tokens[i] == '('){
            startIndex = i;
            for (let j = i+1; j < tokens.length; j++){
                if (tokens[j]== '('){
                    stack.push(tokens[j]);
                } else if (tokens[j] === ')'){
                    if (stack.length === 0){ //have found our matching closing bracket at j
                        let expressionWithinBrackets = tokens.slice(startIndex + 1,j).join(' ');
                        let token = parseProp(expressionWithinBrackets);
                        if (negateNext === true){
                            token = "Not " + token
                            negateNext = false;
                        }
                        queue.push(token);
                        i = j;
                        j = tokens.length; //break the loop
                    }
                    stack.pop();
                }
            }
        }else if (operators.includes(tokens[i])){
            operator = tokens[i];
        }else if (tokens[i] === "Not"){
            negateNext = true;
        }
        else{
            let token = tokens[i];
            if (negateNext === true){
                token = "Not "+ token
                negateNext = false;
            }
            queue.push(token);
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

function parseTreeString(treeString) {
    const stack = [];
    let currentNode = {};
    negateNext = false;

    // Tokenize the treeString into words
    const tokens = treeString.split(/\s+/).filter(Boolean);

    for (let token of tokens) {
        if (token === '(') {
            const newNode = {};
            if (currentNode.children) {
                currentNode.children.push(newNode);
            } else {
                currentNode.children = [newNode];
            }
            stack.push(currentNode);
            currentNode = newNode;
        } else if (token === ')') {
            currentNode = stack.pop();
        } else if (token === 'Not'){
            negateNext = true;
        } 
        else {
            if (negateNext === true){
                negateNext = false;
                currentNode.value = "Not " + token;
            } else{
                currentNode.value = token;

            }
        }
    }

    return currentNode;
}


function generateHTML(node) {
    let html = '';
    if (node.value) {
        html += `<li>${node.value}`;
    } 
    if (node.children && node.children.length > 0) {
        html += '<ul>';
        node.children.forEach(child => {
            html += generateHTML(child);
        });
        html += '</ul>';
    }
    html += '</li>';
    return html;
}

console.log(generateHTML(parseTreeString('( P ( Q ( SAT ) ( UNSAT ) ) ( UNSAT ) )')));

