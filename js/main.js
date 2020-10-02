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
  const worker = new Worker('../fptaylor.js');
  const output = document.getElementById('output');

  const input = document.getElementById('input').value;
  console.log(`input.value = ${input}`);
  
  worker.onmessage = function(e) {
    output.innerHTML += `<div>${e.data}</div>`;
  };

  worker.postMessage(input);
  workers.push(worker);
}

