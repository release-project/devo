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
var eulerText = "";
var width = 800;
var height = 600;
var circleEnum = { TOPOLOGY: 1, ADD : 2, REMOVE : 3};
var circleType = circleEnum.TOPOLOGY;


var svg;
var times = [];

var conn = new WebSocket('ws://localhost:8081');
//var conn = new WebSocket('ws://cs.kent.ac.uk/~rb440/:8081');
conn.onopen = function(e) {
    console.log("Connection established!");
};

conn.onmessage = function(e) {
	if (circleType == circleEnum.TOPOLOGY){
		parseCircles(e.data);
	} else if (circleType == circleEnum.ADD) {
		parseAddSGroupResponse(e.data);
	}
    
};

/** A labelled circle */
function Circle(id,label,r,x,y) {
	this.id = id;
	this.label = label;
	this.r = r;
	this.x = x;
	this.y = y;
	newCircle = false;
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
function findCircleLabel(label){
	for (var i = 0; i < circles.length; i++){
		if (label == circles[i].label){
			return circles[i];
		}
	}
	return null;
}

/*
Finds a circle given a id
 */
function findCircleId(id){
	for (var i = 0; i < circles.length; i++){
		if (id == circles[i].id){
			return circles[i];
		}
	}
	return null;
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
//not used
function generateHash(s){
	return s.split("").reduce(function(a,b){a=((a<<5)-a)+b.charCodeAt(0);return a&a},0);              
}

function next(){
	iterateGraph(nodes, edges);
}

/*
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
}*/

function applyForceModel(){

}

function parseComms(commsFile){

	//var timeInstance = commsFile.split(",!");
	//var timeInstance = commsFile.split(",\n"); //replace when actually running from live stream data
	//var timeInt = parseInt(interactions[0].substring(1));



	var timeInstance = commsFile;
					
	//console.log(input[0], times);	
	//for (var i = 0; i < input.length; i++){
	//for (var i = 0; i < 2; i++){
	//	var timeInstance = input[i];

		//var interactions = timeInstance.split(",\n"); //replace when actually running from live stream data
		var interactions = timeInstance.split(",!");
		
		var timeInt = "";
		//if (i == 0) {
			timeInt = parseInt(interactions[0].substring(1));
		//} else {
		//	timeInt = parseInt(interactions[0].substring(2));
		//}

		var time = new Time(timeInt);
		time.interactions = [];
		d3.select("#time").text(timeInt+" ms");
		//console.log(time, interactions);

		times.push(time);

		for (var j = 1; j < interactions.length; j++){
			var interactionDetails = interactions[j].split(",");

			//console.log(interactionDetails);

			var startAt = interactionDetails[0].indexOf("@");
			var startVal = j == 1 ? 3 : 2;
			var start = interactionDetails[0].substring(startVal,startAt);

			var finishAt = interactionDetails[1].indexOf("@");
			var finish = interactionDetails[1].substring(0,finishAt);


			var count = parseInt(interactionDetails[2]);

			//console.log(time);

			var startNode = findNode(start, nodes);
			var finishNode = findNode(finish, nodes);
			//console.log(startNode, start, nodes);
			var edge = new Edge(startNode, finishNode, count);

			time.interactions.push( edge );

			//console.log(start, finish, count);
		}
		edges = time.interactions;
//	}
	//iterateGraph(nodes, time.interactions);
	drawEdges(time.interactions);
	//console.log(nodes, time.interactions);

}

function parseCircles(input){

	var circleFile = input.split("\n");
	//use 1 as first row of input are labels
	for (var i = 1; i < circleFile.length-1; i++){
		var result = circleFile[i].split(",");
		//console.log(result);

		var c = findCircleId(result[0].trim());
		c.r = parseInt(result[3]);
		c.x = parseInt(result[1]);
		c.y = parseInt(result[2]);
		c.newCircle = false;
		//var c = new Circle(result[0].trim(), , parseInt(result[1]), parseInt(result[2]));
		//circles.push(c);	
	}

	console.log(circles);

	zones = eulerText.split(" ");
	zones.pop();
	console.log(zones);
	rectangles = findZoneRectangles(zones, circles);

	for (var i = 0; i < nodes.length; i++) {
		var n = nodes[i];
		n.region = findRectangleFromLabel(n.regionText, rectangles);
	}

	drawGraph(nodes, null, rectangles, circles);

}


function parseHighTopology(input) {

	if (input == "{s_group_init_config, []}") {
		//no initial configuration
		parseCircles("\n a,"+width/2 + "," + height/2 + "," + height/2);
	} else {
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

		eulerText = "";
		for (var i = 0; i < nodes.length; i++){
			eulerText = eulerText + nodes[i].regionText + " ";
		}

		console.log(eulerText);

		conn.send(eulerText);
	}


}

function parseInput(input){

	if (input.split(",")[2] == "new_s_group"){
		parseAddSGroup(input)
	} else if (input.split(",")[2] == "delete_s_group"){
		parseDeleteSGroup(input)
	} else if (input.split(",")[2] == "add_nodes"){
		parseAddNodes(input)
	} else if (input.split(",")[2] == "remove_nodes"){
		removeNodes(input)
	} else if (input.substring(0,20) == "{s_group_init_config"){
		parseHighTopology(input);
	} else {
		parseComms(input);
	}
}

function parseAddSGroup(input) {
	//{s_group,'node1@127.0.0.1',new_s_group,[{aa, ['node1@127.0.0.1','node2@127.0.0.1']}]}.

	var grpDetails = input.split(",");

	var sgroupName = grpDetails[3].substring(2);
	console.log(sgroupName, grpDetails);

	var id = String.fromCharCode(circles.length + 65);

	var circle = new Circle(id, sgroupName, -1, -1, -1);
	circle.newCircle = true;
	circles.push(circle);

	for (var j = 4; j < grpDetails.length; j++) {
		console.log(grpDetails[j]);
		var rawName = grpDetails[j];
		var at = rawName.indexOf("@");
		
		var start = j == 4 ? 3 : 1
		var nodeName = rawName.substring(start, at);

		var node = findNode(nodeName, nodes);

		if (node == null){
			node = new Node(nodeName, null, id);
			nodes.push(node);
		} else {
			node.regionText = node.regionText + id;
		}

		//if node = null, create new node

		//change node RegionText
		//change node region


		console.log(nodeName, node);
	}

	circleType = circleEnum.ADD;

	eulerText = "";
	for (var i = 0; i < nodes.length; i++){
		//nodes[i].region = findRectangleFromLabel(nodes[i].regionText);
		eulerText = eulerText + nodes[i].regionText + " ";
	}

	console.log(eulerText);

	conn.send(eulerText);

	//recalculate circles
	//recalculate zones

}

function parseAddSGroupResponse(input) {

	console.log(input);

	var circleFile = input.split("\n");
	
	//use 1 as first row of input are labels
	for (var i = 1; i < circleFile.length-1; i++){
		var result = circleFile[i].split(",");
		//console.log(result);

		var c = findCircleId(result[0].trim());

		if (c == null) {

		} else {
			
			c.r = parseInt(result[3]) * multiplier;
			c.x = parseInt(result[1]) * multiplier;
			c.y = parseInt(result[2]) * multiplier;
		}
		
		//var c = new Circle(result[0].trim(), , parseInt(result[1]), parseInt(result[2]));
		
			
		//console.log(c);

	}

	console.log(circles);

	zones = eulerText.split(" ");
	zones.pop();
	console.log(zones);
	rectangles = findZoneRectangles(zones, circles);

	for (var i = 0; i < nodes.length; i++) {
		var n = nodes[i];
		n.region = findRectangleFromLabel(n.regionText, rectangles);
	}

	for (var i = 0; i < circles.length; i++){
		var c = circles[i];
		if (c.newCircle){
			c.newCircle = false;
			addSGroup(c.id);
		} else {
			moveCircle(c.id, c.x, c.y, c.r);
		}
	}
}

function parseDeleteSGroup(input){

	var sGroupName = input.split(",")[3];
	sGroupName = sGroupName.substring(1,sGroupName.length-3);
	
	//remove circle from list
	var circle = findCircleLabel(sGroupName);
	var circleIndex = circles.indexOf(circle);
	//console.log(circles[0], circles[1], circles[2], circleIndex, circle);
	if (circleIndex != -1) {
		circles.splice(circleIndex, 1);
	}
	//console.log(circles[0], circles[1], circles[2]);
	console.log(sGroupName, circle.id);

	var rectangle = findRectangleFromLabel(circle.id, rectangles);

	//console.log(rectangle);

	zones = [];
	//remove this sgroup from nodes
	for (var i = 0; i < nodes.length; i++){
		var node = nodes[i];
	//	console.log(i, node, node.label);
		//this node is only in deleted group, so remove node
		if (node.region == rectangle) {
			//remove node
			removeNode(node); //removes node from svg
			nodes.splice(i, 1);
			i--;
	//		console.log(nodes);
			
		} else {
			//remove this region from node's regionText
			var index = node.regionText.indexOf(circle.id);
			//console.log(node.regionText, index, node.regionText.substring(0,index), node.regionText.substring(index+1));
			var newRegionText = node.regionText.substring(0,index) + node.regionText.substring(index+1);
			node.regionText = newRegionText;

			//build zones, if this region isn't already there
			if (zones.indexOf(newRegionText) == -1) {
				zones.push(newRegionText);
			}
			console.log(node.regionText);
		}

	}
	//console.log(zones, circles);
	//rebuild rectangles
	rectangles = findZoneRectangles(zones, circles);
	
	//console.log (nodes);
	//assign node correct new region
	for (var i = 0; i < nodes.length; i++){
		var node = nodes[i];
		node.region = findRectangleFromLabel(node.regionText, rectangles);
	}
	
	deleteSGroup(circle.id);

	//remove svg of circle

	console.log(sGroupName, circle.id, zones, rectangles, circles);

	//{s_group, CurrentNode, delete_s_group, [Nodes]}

//e.g. {s_group,'node1@127.0.0.1',delete_s_group,[aa]}.
}

function parseAddNodes(input) {

//{s_group,'node1@127.0.0.1',add_nodes,[aa,['node3@127.0.0.1']]}.

	var circleLabel = input.split(",")[3].substring(1);
	var circle = findCircleLabel(circleLabel);
	var rectangle = findRectangleFromLabel(circle.id, rectangles);

	var nodesArr = input.split(",");
	for (var i = 4; i < nodesArr.length; i++){
		//console.log(nodesArr[i]);
		var rawNode = nodesArr[i];

		var start = 1;
		var finish = rawNode.indexOf("@");

		if (i == 4){
			start = 2;
		}
		var nodeName = rawNode.substring(start, finish);
		var node = new Node(nodeName, rectangle, circle.id);
		nodes.push(node);
		node.x = findNodeStartX(node, nodes.length, false);
		node.y = findNodeStartY(node, nodes.length, false);
		
		addNode(node);

		console.log(nodeName, rectangle);
	}


}