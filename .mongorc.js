DBQuery.prototype._prettyShell = true
EDITOR="vim";

var host = "@";
var status = db.serverStatus();
if (!!status){
  host += status.host.split('.')[0];
  host += ".";
}
var username = "anon";
var connectSt = db.runCommand({connectionStatus : 1});
if (!!connectSt){
  var user = connectSt.authInfo.authenticatedUsers[0];
  if (!!user){
    username = user.user;
  }
}
var current_db = db.getName();

var prompt = function() {
  var def = username + host + current_db + " > ";
  if(!!status){
    db.setProfilingLevel(2);
  }
  var obj = db.getLastErrorObj();
  if (obj.err) {
    return "Error: " + obj.err + "\n> ";
  }
  return def;
};
