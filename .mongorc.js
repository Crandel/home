// DBQuery.prototype._prettyShell = true
EDITOR="vim";

var host = db.serverStatus().host.split('.')[0];
var username = "anon";
var user = db.runCommand({connectionStatus : 1}).authInfo.authenticatedUsers[0];
if (!!user){
  username = user.user;
}
var current_db = db.getName();

var prompt = function() {
  var def = username + "@" + host + "." + current_db;
  var ndocs, obj, p, sec;
  ndocs = function(n) {
    return def + n + " document" + (n > 1 ? 's' : '');
  };
  db.setProfilingLevel(2);
  obj = db.getLastErrorObj();
  pq = db.system.profile.find({
    $and: [
      {ns: {$not: /profile$/}},
      {ns: {$not: /\$cmd$/}},
      {ns: {$not: /indexes$/}}
    ]
  }).sort({ts: -1}).limit(1);
  if (pq.count() > 0){
    var p = pq[0]
    sec = "" + (p.millis / 1000) + " sec";
    if (obj.err) {
      return "Error: " + obj.err + "\n> ";
    }
    switch (p.op) {
    case 'insert':
      return "Query OK, 1 document inserted (" + sec + ") > ";
    case 'query':
      return ">= " + (ndocs(p.nreturned)) + " in set (" + sec + ") > ";
    case 'remove':
      return "Query OK, x documents removed (" + sec + ") > ";
    case 'update':
      return "Query OK, " + (ndocs(p.nupdated)) + " affected (" + sec + ") > ";
    }
  }

  return def + " > ";
};
