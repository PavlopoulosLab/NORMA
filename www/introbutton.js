document.addEventListener('DOMContentLoaded', function() {
	var selector = document.getElementById("btnAddNetwork");
		selector.setAttribute("data-step", "1");
		selector.setAttribute("data-intro", "The network file is an obligatory, 2-column (or 3-column for weighted), tab-delimited file, containing all network connections of an undirected network. This file must contain headers, namely: 'Source' and 'Target'. Press this button to upload your network file");
		selector.setAttribute("data-position", "bottom");
		
		selector = document.getElementById("uiLoadGraphOptionsOutput");
		selector.setAttribute("data-step", "2");
		selector.setAttribute("data-intro", "If the network is weighted (3 columns), check this box to allow weighted visualization.");
		selector.setAttribute("data-position", "bottom");
		
		selector = document.getElementById("btnAddNetwork2");
		selector.setAttribute("data-step", "3");
		selector.setAttribute("data-intro", "The annotation file is an obligatory, 2-column, tab-delimited file which contains information about the defined groups. The first column contains the group names whereas the second column contains the node names in a group separated by a comma (,) and without spaces. No headers are allowed. Press this button to upload your annotation file");
		selector.setAttribute("data-position", "bottom");
		
		selector = document.getElementById("btnAddExpression");
		selector.setAttribute("data-step", "4");
		selector.setAttribute("data-intro", "The expression file is an optional, 2-column, tab-delimited file to allow node coloring (e.g. gene expressions). The first column contains the node names and the second column the node colors (color names or hex codes (e.g. Green or #00ff00, Red or #ff0000, Yellow or #ffff00)). Press this button to upload your node - coloring file");
		selector.setAttribute("data-position", "bottom");

		selector = document.getElementById("introButton");
		selector.setAttribute("data-step", "5");
		selector.setAttribute("data-intro", "For more details, please check Help Pages tab.");
		selector.setAttribute("data-position", "bottom");
	
	document.getElementById("introButton").onclick = function(){
		introJs().start();
	};
}, false);
