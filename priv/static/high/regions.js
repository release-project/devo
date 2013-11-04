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

/** A labelled circle */
function Circle(label,r,x,y) {
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


function Node(label,region){
	this.label = label;
	x = 0.0;
	y = 0.0;
	this.region = region;
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