// Connect to the WebSocket server
const socket = new WebSocket('ws://localhost:8080');

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
    outputDiv.innerHTML = "<h2>Truth Table</h2>";
    outputDiv.innerHTML += createTableFromData(data);
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

// Function to send a proposition to the server
function sendProposition() {
    const expression = document.getElementById('expressionInput').value;
    const parsedExpression = parseExpression(expression);
    console.log(parsedExpression);
    socket.send(parsedExpression);
}

// Function to parse the expression and convert it into the desired format
function parseExpression(expression) {
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
        // If the token is 'AND', 'OR', 'NOT', or 'IMPLIES', return the uppercase version
        if (['AND', 'OR', 'NOT', 'IMPLIES'].includes(token.toUpperCase())) {
            return token;
        }
        // Otherwise, assume it's a variable and return it wrapped with 'Var'
        return `Var '${token}'`;
    });

    // Join the mapped tokens with spaces to form the final expression
    return mappedTokens.join(' ');
}