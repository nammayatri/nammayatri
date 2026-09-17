import * as notifier from "node-notifier";
const args = process.argv.slice(2);

try {
  if (args[0] == "FAILURE")
    notifier.notify({
      title: "Build Failure",
      message: "Build Failed!!! Code Better",
      sound: true,
      icon: "Terminal Icon"
    });
  else
    notifier.notify({
      title: "Build Success",
      message: "Build Passed!!! Remove logs now",
      sound: true,
      icon: "Terminal Icon"
    });
} catch (error) {
  console.log("Notifier error")
}

