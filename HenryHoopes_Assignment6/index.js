/*
Program Name: EECS 468 Assignment 6
Description: This JS file contains the code for creating a server that listens for http requests. 
	Clients can use http request with GET, PUT, DELETE, and MKCOL to get files, save files,
	delete files/directories, and make directories.
Inputs: http request with correct method and body for necessary function
Outputs: http response with data requested or status indicating function complete/error
Full Name: Henry Michael Hoopes
Collaborators: While on replit, there was an autocomplete functionality that kept trying to complete
	my code. Most of the time, it didn't know what it was doing. I only used it to complete comments, 
	but I thought it worthy to note here.
Creation: 03/25/2024
Link to REPLIT: https://replit.com/join/atgikkmzle-hmhoopes
*/


//uses require to obtain createServer function from http module
const {createServer} = require("http");

//creates an empty array of methods to start
const methods = Object.create(null);

//calls create server method, which performs code in following block
//when a request is made to port the server is listening to
createServer((request, response) => {
	
	//stored the value returned by indexing methods array with method of request 
	//in handler.If no method is found, stores notAllowed
	let handler = methods[request.method] || notAllowed;
	
	//Calls function stored within handler with argument request
	handler(request)
		
	//if returned promise rejects, run the following
	.catch(error => {
		//if the returned result has an error status, return that result
		if (error.status != null) return error;
		
		//if the returned result has no error status, set body to 'error' and 
		//error code to 500, indicating unknown error
		return {body: String(error), status: 500};
	//if the returned promise resolves, run the following, with status set 
	//to 200 and type to plain text unless set in thefunction of handler
	}).then(({body, status = 200, type = "text/plain"}) => {
		//Set status and content-type in head of server's response
		//to vals stored in status and type
		response.writeHead(status, {"Content-Type": type});
		
		//if the body.pipe function exists (means body is readable stream), use 
		//pipe method to set content of body to writeable stream of response.
		if (body && body.pipe) body.pipe(response);
			
		//if body isn't readable stream (if null/string/buffer), it is directly 
		//used as the body of response by calling response.end
		else response.end(body);
	});
	//sets server to listen/be active on port 8000
}).listen(8000);

//defines notAllowed to take a request as an argument and return promise
async function notAllowed(request) {
	//returns the following block of a code as a promise
	return {
		//sets status of response as 405 (method not allowed)
		status: 405,
		//sets text to indicate method wasn't allowed
		body: `Method ${request.method} not allowed.`
	};
}

//includs createReadStream function from filesystem module
const { createReadStream } = require("fs");
//includes stat and readdir as promise functions from filesystem module
const { stat, readdir } = require("fs").promises;
//needed to alter this line slightly as require(mime) was causing error
const mime = require("mime-types")
//stores GET in methods as function that takes a request and returns a promise 
methods.GET = async function (request) {
	//stores pathname of request as file path
	let path = urlPath(request.url);
	//creates var stats
	let stats;
	//uses try catch block to store result of stat function in stats
	try {
		//stores result of stat (wether or not path is file or directory)
		stats = await stat(path);
	//if stat function throws an error, run the following
	} catch (error) {
		//if error is not file not found error, throw error
		if (error.code != "ENOENT") throw error;
		//else return status and body indicating file not found
		else return { status: 404, body: "File not found" };
	}
	//if path is a directory, run the following
	if (stats.isDirectory()) {
		//uses readdir function to read all files in directory and return it as body of result
		return { body: (await readdir(path)).join("\n") };
	//if path is a file, run the following
	} else {
		//return body as readableStream from the file and type as mime type of file
		return { body: createReadStream(path), type: mime.lookup(path)};
	}
};

//includes parse function from url module
const { parse } = require("url");
//includes resolve function and sep operator from path module
const { resolve, sep } = require("path");
//sets the baseDirectory to the current working directory
const baseDirectory = process.cwd();
//creates helper function that takes in url and return file path
function urlPath(url) {
	//stores result of parse function when given url
	let { pathname } = parse(url);
	//the following 4 lines of code are used to check that the path given
	//is under the current working directory. If it isn't, returns a 403 forbidden message
	let path = resolve(decodeURIComponent(pathname).slice(1));
	if (path != baseDirectory && !path.startsWith(baseDirectory + sep)) {
		throw { status: 403, body: "Forbidden" };
	}
	//if path is under current working directory, return it
	return path;
}

//include rmdir and unlink functions from filesystem module
const { rmdir, unlink } = require("fs").promises;
//create function that takes request and returns promise and adds it to list of valid methods for DELETE
methods.DELETE = async function (request) {
	// translate the url into a file name
	let path = urlPath(request.url);
	// invoke stat object called stats
	let stats;
	// wait for stat to find the file type (file or directory)
	try {
		stats = await stat(path);
	// handle a non-existent file name
	} catch (error) {
		//if error isn't file not found, throw error
		if (error.code != "ENOENT") throw error;
		//if file isn't found, return success (b/c it was deleted)
		else return { status: 204 };
	}
	// if the file name is a directory, remove it
	if (stats.isDirectory()) await rmdir(path);
	// if the file name is not a directory, remove it
	else await unlink(path);
	// report that the file deletion was successful
	return { status: 204 };
};

//include createWriteStream function from filesystem module
const { createWriteStream } = require("fs");
//create helper function pipeStream that takes in readable stream and writestream and converts readstream into writestream
function pipeStream(from, to) {
	//return new promise 
	return new Promise((resolve, reject) => {
		//when error occurs on readstream, reject promise
		from.on("error", reject);
		//when error occurs on writestream, reject promise
		to.on("error", reject);
		//when writestream is finished, resolve promise
		to.on("finish", resolve);
		//call pipe function to convert readstream to writestream
		from.pipe(to);
	});
}

//create function that takes in request and returns promise and adds it to list of valid methods for PUT
methods.PUT = async function (request) {
	//stores pathname of request as file path
	let path = urlPath(request.url);
	//waits for pipestream function to convert request to written file
	await pipeStream(request, createWriteStream(path));
	//return success
	return { status: 204 };
};

//-------------BEGIN MY CODE----------------
//inclue promise version of mkdir function from file system module
const { mkdir } = require("fs").promises;
//create MKCOL method that returns a promise and add it to list of valid methods
methods.MKCOL = async function (request) {
	//translate the url into a file path using urlPath function
	let path = urlPath(request.url);
	//use try block to wait for mkdir to create the directory
	try {
		await mkdir(path);
	//use catch part of block to catch any error
	} catch (error) {
		//if the error isn't an ENOENT error (file not found), throw it
		if (error.code != "ENOENT") throw error;
		//if the error is a file not found error, return body and status indicating this 
		else return { status: 404, body: "File not found" };
	}
	//if the mkdir completed w/o error, return status indicating success
	return { status: 204 };
};

