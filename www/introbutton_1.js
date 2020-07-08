document.addEventListener('DOMContentLoaded', function() {
	var selector = document.getElementById("btnAddNetwork");
		selector.setAttribute("data-step", "1");
		selector.setAttribute("data-intro", "Network file is an obligatory, 2-column (or 3-column if it is weighted), tab-delimited file, containing all network connections of an undirected network. This file must contain headers, namely: 'Source' and 'Target'. Notably, self-loops and multiple-edges are eliminated automatically. Press this button to upload your network file");
		selector.setAttribute("data-position", "bottom");
		
		selector = document.getElementById("uiLoadGraphOptionsOutput");
		selector.setAttribute("data-step", "2");
		selector.setAttribute("data-intro", "If network is weighted (network file consists of 3 columns), check this box to allow weighted visualization of the network.");
		selector.setAttribute("data-position", "bottom");
		
		selector = document.getElementById("btnAddNetwork2");
		selector.setAttribute("data-step", "3");
		selector.setAttribute("data-intro", "Annotation file is an obligatory, 2-column, tab-delimited file which contains information about the defined groups. The first column contains the group names whereas the second column contains the node names in a group separated by a comma (,) and without spaces. No headers are allowed. Press this button to upload your annotation file");
		selector.setAttribute("data-position", "bottom");
		
		selector = document.getElementById("btnAddExpression");
		selector.setAttribute("data-step", "4");
		selector.setAttribute("data-intro", "Expression file is an optional, 2-column, tab-delimited file which contains information about node coloring (e.g. gene expressions). The first column contains the node names and the second column the node colors (color names or hex codes (e.g. Green / #00ff00, Red / #ff0000, Yellow / #ffff00, etc.)). Press this button to upload your expression file");
		selector.setAttribute("data-position", "bottom");

		selector = document.getElementById("introButton");
		selector.setAttribute("data-step", "5");
		selector.setAttribute("data-intro", "For more details, please check Help Pages tab.");
		selector.setAttribute("data-position", "bottom");
	
	document.getElementById("introButton").onclick = function(){
		introJs().start();
	};
}, false);
