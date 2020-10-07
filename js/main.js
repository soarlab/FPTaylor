const classes = {1: 'stdout', 2: 'stderr'};

const workers = new Set();

function clearOutput() {
  const output = document.getElementById('output');
  output.innerHTML = '';
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

