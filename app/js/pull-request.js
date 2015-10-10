function writeToConsole( content ) {
    console.log(content);
}

function submitHaskAnythingPullRequest( userToken, userName, data ) {
    // We'll use this as a simple log.
    var logWriter = writeToConsole;

    // Open up the connection to Github.
    var github = new Github({
        token: userToken,
        auth: "oauth"
    });
    
    logWriter("Opened up a connection to Github.");
    
    // This is the repository that will be forked and a pull request submitted against.
    var master = { username: "beerendlauwers", repo: "hask-anything" };
    
    var masterHaskAnything = github.getRepo(master.username, master.repo);
    
    // First, check if we've already forked it.
    var maybeFork = github.getRepo(userName, master.repo);
    
    logWriter("Checking if we've already forked repository " + master.repo + " from " + master.username + " ...");
    
    maybeFork.show( function(error,repo) {
        if (error) {
            // No such repository is known, so let's fork it and restart.
            logWriter("No fork found. Forking...");
            
            masterHaskAnything.fork( function(error) {
                if (error) {
                    logWriter("Something went wrong during the forking of the " + master.repo + " repository. The error data follows:");
                    logWriter(error);
                }
            });
            
            logWriter("Forked repository " + master.repo + " from " + master.username + ".");
            logWriter("We'll start again from the top in three seconds.");
            logWriter("3..");
            setTimeout( function() { logWriter("2.."); }, 1000 );
            setTimeout( function() { logWriter("1.."); }, 2000 );
            setTimeout( function() { submitHaskAnythingPullRequest( userToken, userName, data ); }, 3000 );
            return;
        }
        else if (repo !== undefined) {
            // Repository seems to be forked.
            logWriter("Repository seems to be forked.");
            
            var justFork = maybeFork;
            
            logWriter("Checking if we've already written this data to this file...");
            
            // Check if we've already committed these *exact* file contents to this *exact* file name.
            justFork.read('master', data.fileName, function( error, result ) {
                var skipCommit = false;
            
                if (error && error !== "not found") {
                    logWriter("Something went wrong during the writing of the commit. The error data follows:");
                    logWriter('"' + error + '"');
                    return;
                }
                else if (result && result === data.fileContents) {
                    logWriter("File \"" + data.fileName + "\" already contains this data. Skipping the commit.");
                    skipCommit = true;
                }
                
                var doPullRequest = function() {
                    // Ok, create a pull request for this commit.
                    var pull = {
                      title: data.pullRequestName,
                      body: data.pullRequestBody,
                      base: "master",
                      head: userName + ":" + "master"
                    };
                    
                    logWriter("Creating pull request \"" + pull.title + "\"...");
                    
                    masterHaskAnything.createPullRequest( pull, function(error, pullRequest) {
                        if (error) {
                            logWriter("Something went wrong during the creation of the pull request. The error data follows:");
                            
                            if (error.error == 422) {
                                logWriter(error.request.responseText);
                            }
                            
                            logWriter(error); 
                        }
                        else {
                            logWriter("Pull request \"" + pull.title + "\" created.");
                            logWriter("Done!");
                        }
                    });
                }
                    
                if (!skipCommit) {
                    // Let's write a commit to the forked repository now.
                    logWriter("Writing the commit " + data.commitMessage + " to the master branch of the fork...");
                    
                    justFork.write('master', data.fileName, data.fileContents, data.commitMessage, function(err) {
                        if (error && error !== "not found") {
                            logWriter("Something went wrong during the writing of the commit. The error data follows:");
                            logWriter(error);
                            return;
                        }
                        else {
                            logWriter("Commit " + data.commitMessage + " written.");
                            doPullRequest();
                        }
                    });
                }
                else {
                    doPullRequest();
                }
            });
            
        }
        else {
            logWriter("Something went wrong during while checking if the " + msater.repo + " repository was forked. The error data follows:");
            logWriter(error);
        }
    });
}

function testSubmit() {
    var hask = {
        commitMessage: "Testing the entire flow",
        fileName: "flowtest.md",
        fileContents: "------\ntitle: Testing the flow\n------\nThis should work",
        pullRequestName: "Automated pull request with flow",
        pullRequestBody: "Body of the pull request."
    };
}