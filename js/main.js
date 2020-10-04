const classes = {1: 'stdout', 2: 'stderr'};

const workers = [];

function onClear() {
  const output = document.getElementById('output');
  output.innerHTML = '';
}

function onStop() {
  workers.forEach(worker => worker.terminate());
  workers = [];
}

function callFPTaylor() {
  const worker = new Worker('fptaylor.js');
  const output = document.getElementById('output');

  const input = document.getElementById('input').value;
  const config = document.getElementById('config').value;
  
  worker.onmessage = function(e) {
    const {ty, str} = e.data;
    output.innerHTML += `<div class="${classes[ty]}">${str}</div>`;
  };

  output.innerHTML = '';

  worker.postMessage({input, config});
  workers.push(worker);
}

