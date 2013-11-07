var circles = [];
var nodes = [];
var edges = [];
var zones = [];
var multiplier = 3;
var c = 20;
var t = 200;
var k;
var rectangles;
var currentTime = 0;

var svg;
var times = [];

var conn = new WebSocket('ws://localhost:8081');
//var conn = new WebSocket('ws://cs.kent.ac.uk/~rb440/:8081');
conn.onopen = function(e) {
    console.log("Connection established!");
};

conn.onmessage = function(e) {
    parseCircles(e.data);
};

/** A labelled circle */
function Circle(id,label,r,x,y) {
	this.id = id;
	this.label = label;
	this.r = r;
	this.x = x;
	this.y = y;
}


function Rectangle(x,y,width,height) {
	this.x = x;
	this.y = y;
	this.width = width;
	this.height = height;
	this.label = "";
}

function Point(x,y) {
	this.x = x;
	this.y = y;
}


function Node(label,region, regionText){
	this.label = label;
	x = 0.0;
	y = 0.0;
	this.region = region; //rectangle
	this.regionText = regionText;
	horizontal = 0;
	vertical = 0;
}

function Edge(source, target, size){
	this.source = source;
	this.target = target;
	this.size = size;
}


function Time(time){
	this.time = time;
	this.interactions = [];
}

function Interaction(start, end, size){
	this.start = start;
	this.end = end;
	this.size = size;
}


/*
Finds a circle given a label
 */
function findCircle(label){
	for (var i = 0; i < circles.length; i++){
		if (label == circles[i].label){
			return circles[i];
		}
	}
}

/*
Finds a rectangel given a label
*/
function findRectangleFromLabel(label, rectangles){

	for (var i = 0; i < rectangles.length; i++){
		if (label.trim() == rectangles[i].label.trim()){
			return rectangles[i];
		}
	}
}

/*
Finds a node given a label
*/
function findNode(label, nodes){
	for (var i = 0; i < nodes.length; i++){
		//console.log(nodes[i].label,label, nodes[i].label == label);
		if (nodes[i].label == label) {
			return nodes[i];
		}
	} 
	return null;
}

function stringCompare(s1, s2){
	console.log(s1.length,s2.length, s1.charAt(1));
	if (s1.length != s2.length){
		return false;
	}
	for (var i = 0; i < s1.length; i++){
		//console.log(s1.charAt(i),s2.charAt(i));
		if (s1.charAt(i) != s2.charAt(i)){
			return false;
		}
	}
	return true;

}

// Generates a hash for creating unqiue circle or node IDs
function generateHash(s){
	return s.split("").reduce(function(a,b){a=((a<<5)-a)+b.charCodeAt(0);return a&a},0);              
}

function next(){
	iterateGraph(nodes, times[currentTime].interactions);
}


function animate(){
	var m = 0;
	var changeValue = 100;
	$("#time").html(times[currentTime].time+" ms");
	var interval = setInterval(
		function() {
			m++;
			//console.log("m", m, "currentTime", currentTime);
			if (m % changeValue == changeValue-1) {
				currentTime++;
				$("#time").html(times[currentTime].time+" ms");
				if (currentTime == times.length){
					window.clearInterval(interval);
					console.log("done");
					return;
				}
				drawEdges(times[currentTime].interactions);
			}
			next();
		}
	,10);
}


/**
*	To be replaced with actual circle calculation
*/
function prepareHighLevel(){

	var a = new Circle(a, 99.97890081940014, 127.40276713478325, 63.29113497213361);
	var c = new Circle(c, 68.33333333333333, 72.59103641456582, 63.29113497213361);
	var b = new Circle(b, 131.62446830546693, 72.59103641456582, 63.29113497213361);
	circles.push(a);circles.push(b);circles.push(c);

	console.log(circles);

	//to be replaced with actual node information
	zones = ["a", "ab", "c"];
	rectangles = findZoneRectangles(zones, circles);

	var nodesSelected = $("#nodeList").val();
	for (var i = 0; i < nodesSelected.length; i++){
		var node = new Node(nodeLabel, findRectangleFromLabel(zones[i], rectangles));
		nodes.push(node);
	}
	
	console.log(circles, nodes, rectangles);
}

function parseComms(commsFile){
	var input = commsFile;
					
	//console.log(input[0], times);	
	for (var i = 0; i < input.length; i++){
	//for (var i = 0; i < 2; i++){
		var timeInstance = input[i];

		var interactions = timeInstance.split(",\n");
		
		var timeInt = "";
		if (i == 0) {
			timeInt = parseInt(interactions[0].substring(1));
		} else {
			timeInt = parseInt(interactions[0].substring(2));
		}

		var time = new Time(timeInt);
		times.push(time);

		for (var j = 1; j < interactions.length; j++){
			var interactionDetails = interactions[j].split(",");

			var start = parseInt(interactionDetails[0].substring(8));
			var finish = parseInt(interactionDetails[1].substring(4));
			var count = parseInt(interactionDetails[2]);

			//console.log(time);

			var startNode = findNode("node"+start, nodes);
			var finishNode = findNode("node"+finish, nodes);
			//console.log(startNode, start, nodes);
			var edge = new Edge(startNode, finishNode, count);

			time.interactions.push( edge );

			//console.log(start, finish, count);
		}

	}
	iterateGraph(nodes, time.interactions);
	//console.log(times);

}

function parseCircles(input){

	var circleFile = input.split("\n");
	//use 1 as first row of input are labels
	for (var i = 1; i < circleFile.length-1; i++){
		var result = circleFile[i].split(",");
		//console.log(result);
		var c = new Circle(result[0].trim(), parseInt(result[3]), parseInt(result[1]), parseInt(result[2]));
		circles.push(c);	
	}

	console.log(circles);

	zones = ["a", "ab", "c"];
	rectangles = findZoneRectangles(zones, circles);

}


function parseHighTopology(input) {
	var grpText = input.split("{");
	for (var i = 2; i < grpText.length; i++){
		var grpDetails = grpText[i].split(",");
		var grpName = grpDetails[0];

		var id = String.fromCharCode(circles.length + 65);
		var c = new Circle(id, grpName, 0, 0, 0);
		circles.push(c);

		console.log(grpName, id, c);

		for (var j = 1; j < grpDetails.length; j++){
			var rawNodeName = grpDetails[j];
			if (rawNodeName == "") {
				continue;
			}
			var at = rawNodeName.indexOf("@");
			var start = j==1 ? 2 : 1;
			var nodeName = rawNodeName.substring(start,at);

			console.log(nodeName);

			var nodeFound = findNode(nodeName, nodes);
			if (nodeFound == null) {
				var node = new Node(nodeName, null, id);
				nodes.push(node);
			} else {
				nodeFound.regionText = nodeFound.regionText+id;
				console.log("node found");
			}
		}

	}
	console.log(circles, nodes);

}
