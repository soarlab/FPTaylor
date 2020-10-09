const classes = {1: 'stdout', 2: 'stderr'};

const workers = new Set();

function clearOutput() {
  const output = document.getElementById('output');
  output.innerHTML = '';
  const results = document.getElementById('results');
  results.innerHTML = '';
}

function stopAllWorkers() {
  workers.forEach(worker => worker.terminate());
  workers.clear();
}

function onRunButton() {
  const runButton = document.getElementById('run');
  if (workers.size) {
    stopAllWorkers();
    runButton.innerText = 'Run'
  }
  else {
    runFPTaylor(() => !workers.size && (runButton.innerText = 'Run'));
    runButton.innerText = 'Stop';
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

function openTab(event, tabId) {
  const tabs = document.getElementsByClassName('tabcontent');
  for (const element of document.getElementsByClassName('tabcontent')) {
    element.style.display = element.id === tabId ? 'block' : 'none';
  }
  for (const element of document.getElementsByClassName('tablink')) {
    element.classList.remove('active');
  }
  event.currentTarget.classList.add('active');
}

window.onload = function() {
  populateSelect(inputExamples, 'input-examples', 'input', 'user-input');
  populateSelect(configExamples, 'config-examples', 'config', 'user-config');
  document.getElementById('output-tablink').click();
}
