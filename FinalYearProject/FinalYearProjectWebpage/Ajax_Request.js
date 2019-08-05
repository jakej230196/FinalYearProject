// Function to retreive data from database with Ajax
function Ajax_Get(String_Param){
    var Ajax_Request = $.ajax({
	// String_Param is the name of the php file needed.
    method: "GET",
    url: String_Param,
   // dataType: "string",
    // asynch off
    async: false
  });

// Return the ajax object
return Ajax_Request.responseText;

}


