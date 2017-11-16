DBQuery.prototype._prettyShell = true
EDITOR="vim";

var host = db.serverStatus().host.split('.')[0];
var username = "anon";
var user = db.runCommand({connectionStatus : 1}).authInfo.authenticatedUsers[0];
if (!!user){
  username = user.user;
}
var current_db = db.getName();

var prompt = function() {
  var def = username + "@" + host + "." + current_db + " > ";
  db.setProfilingLevel(2);
  var obj = db.getLastErrorObj();
  if (obj.err) {
    return "Error: " + obj.err + "\n> ";
  }
  return def;
};
