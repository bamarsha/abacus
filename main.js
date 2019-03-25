const { BrowserWindow, app, ipcMain } = require("electron");
const freeport = require("freeport");
const spawn = require("child_process").spawn;
const path = require("path");
const readline = require("readline");

// Starts the Threepenny server.
function startServer(port) {
  const server = spawn(path.join(__dirname, "abacus-web"), [port]);
  server.stdout.setEncoding("utf8");
  server.stderr.setEncoding("utf8");
  server.stdout.on("data", console.log);
  server.stderr.on("data", console.log);
  server.on("close",
            code => console.log(`Threepenny server exited with code ${code}`));

  // Kill the server when quitting Electron.
  app.on("will-quit", () => server.kill());

  return server;
}

app.on("ready", () => {
  let window = new BrowserWindow({
    width: 330,
    height: 350,
    minWidth: 260,
    minHeight: 350,
    useContentSize: true,
    title: "Abacus",
    show: false,
    webPreferences: { nodeIntegration: true }
  });
  ipcMain.on("did-setup", () => window.show());

  // Find a random port to run on.
  freeport((error, port) => {
    if (error) throw error;

    const server = startServer(port);

    // Wait for the server to start, then open it in the window.
    const rl = readline.createInterface({
      input: server.stderr
    });
    rl.on("line", function waitForListen(line) {
      if (line.startsWith("Listening on ")) {
        const url = line.substring("Listening on ".length);
        console.log(`Loading URL: ${url}`);
        window.loadURL(url);
        rl.removeListener("line", waitForListen);
      }
    });
  });
});

// Quit when all windows are closed.
app.on("window-all-closed", () => app.quit());
