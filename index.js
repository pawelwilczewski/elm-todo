const app = Elm.Main.init({
  node: document.getElementById("root"),
});

app.ports.saveTodos.subscribe(function (todos) {
  localStorage.setItem("todos", JSON.stringify(todos));
});

const stored = localStorage.getItem("todos");
if (stored) {
  try {
    const todos = JSON.parse(stored);
    app.ports.loadTodos.send(todos);
  } catch (e) {
    app.ports.loadTodos.send([]);
  }
} else {
  app.ports.loadTodos.send([]);
}
