const classes = {1: 'stdout', 2: 'stderr text-danger'};

const workers = new Set();

function clearOutput() {
  const output = document.getElementById('output');
  output.innerHTML = '';
  const results = document.getElementById('results-tbody');
  results.innerHTML = '';
}

function stopAllWorkers() {
  workers.forEach(worker => worker.terminate());
  workers.clear();
}

function onRunButton() {
  const runButton = document.getElementById('run');
  function restoreRunButton() {
    runButton.innerText = 'Run';
    runButton.classList.remove('w3-red');
    runButton.classList.remove('btn-danger');
  }
  if (workers.size) {
    stopAllWorkers();
    restoreRunButton();
  }
  else {
    runFPTaylor(() => !workers.size && restoreRunButton());
    runButton.innerText = 'Stop';
    runButton.classList.add('btn-danger');
    runButton.classList.add('w3-red');
  }
}

function displayResults(data) {
  const results = document.getElementById('results-tbody');
  function addCell(row, value) {
    row.insertCell().appendChild(document.createTextNode(value || '-'));
  }
  for (const res of data) {
    const row = results.insertRow();
    addCell(row, res.name);
    addCell(row, res.absErrorExactStr || res.absErrorApproxStr);
    addCell(row, res.relErrorExactStr || res.relErrorApproxStr);
    addCell(row, res.ulpErrorExactStr || res.ulpErrorApproxStr);
    addCell(row, res.elapsedTime.toFixed(2) + 's');
    // console.log(res);
  }
}

function runFPTaylor(onFinished) {
  const worker = new Worker('fptaylor.js');
  workers.add(worker);
  const output = document.getElementById('output');

  const input = document.getElementById('input').value;
  const config = document.getElementById('config').value;
  
  worker.onmessage = function(e) {
    if (Array.isArray(e.data)) {
      // Final results
      displayResults(e.data);
      workers.delete(worker);
      if (onFinished) onFinished(e.data);
    }
    else {
      const {ty, str} = e.data;
      output.innerHTML += `<div class="${classes[ty]}">${str}</div>`;
    }
  };

  output.innerHTML = '';
  worker.postMessage({input, config});
}

function populateSelect(examples, selectId, inputId, localId) {
  const select = document.getElementById(selectId);
  const input = document.getElementById(inputId);
  function addOption(example) {
    const opt = document.createElement('option');
    opt.textContent = example.name;
    opt.value = example.data.trim();
    select.appendChild(opt);
  }
  addOption({name: '--', data: ''});
  examples.forEach(addOption);
  select.onchange = function(e) {
    if (e.target.value) input.value = e.target.value;
  };
  input.oninput = function() {
    select[0].selected = true;
  }
  input.onchange = function() {
    if (localId) {
      localStorage.setItem(localId, input.value);
    }
  }

  const userInput = localId ? localStorage.getItem(localId) : null;
  if (userInput) {
    select[0].selected = true;
    input.value = userInput;
  }
  else {
    select[1].selected = true;
    input.value = examples[0].data;
  }
}

function switchTheme() {
  const root = document.documentElement;
  const dark = root.hasAttribute('dark');
  if (dark) {
    root.removeAttribute('dark');
  }
  else {
    root.setAttribute('dark', '');
  }
}

function openTab(tabs, tabId) {
  for (const {tab, content} of tabs) {
    if (tab.id === tabId) {
      if (content) content.style.display = 'block';
      tab.classList.add('tab-active');
    }
    else {
      if (content) content.style.display = 'none';
      tab.classList.remove('tab-active');
    }
  }
}

function initTabs(tabsContainer) {
  const tabs = [];
  for (const tab of tabsContainer.getElementsByClassName('tab-item')) {
    const content = document.getElementById(tab.id.slice(4));
    tab.onclick = () => openTab(tabs, tab.id);
    tabs.push({tab, content});
  }
}

window.onload = function() {
  populateSelect(inputExamples, 'input-examples', 'input', 'user-input');
  populateSelect(configExamples, 'config-examples', 'config', 'user-config');

  for (const el of document.getElementsByClassName("tabs")) {
    initTabs(el);
  }

  document.getElementById('run').onclick = onRunButton;
  document.getElementById('clear').onclick = clearOutput;

  let el = document.getElementById('tab-output');
  if (el) el.click();
}
