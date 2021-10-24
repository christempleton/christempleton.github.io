function setAllPopup() {
	elList = document.querySelectorAll('a[rel="ewpopup"]');
	
	for(var i = 0; i < elList.length; i++) {
		
		var anchor = elList[i];
		
		anchor.addEventListener("click", function(evt) {
			 
       		 event.preventDefault();
       		
       		let inUrl = this.getAttribute('href');
			let intWidth = getUrlParam(inUrl, "width", 300);
			let intHeight = getUrlParam(inUrl, "height", 300);
			
			BigPicture({
				el: event.target,
				iframeSrc: inUrl,
				dimensions: [intWidth, intHeight],
			})
			return true;
		}, true);
	}
 
}

function getUrlParam(strUrl, parameter, defaultvalue){
	var urlparameter = defaultvalue;
	if(strUrl.indexOf(parameter) > -1){
		urlparameter = getUrlVars(strUrl)[parameter];
		}
	return urlparameter;
}

function getUrlVars(strUrl) {
	var vars = {};
	var parts = strUrl.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
		vars[key] = value;
	});
	return vars;
}

if(window.attachEvent) {
    window.attachEvent('onload', setAllPopup);
} else {
    if(window.onload) {
        var curronload = window.onload;
        var newonload = function(evt) {
            curronload(evt);
            setAllPopup(evt);
        };
        window.onload = newonload;
    } else {
        window.onload = setAllPopup;
    }
}

