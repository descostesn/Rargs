# This function retrieves the values for each parameter put on the command line
# All succesfully processed parameters will be assigned to global environment
# IMPORANT : This cannot handle parameters with no value (put a bool if needed)
# Everything before the first parameter on the command line will be ignored
# This can handle several values for an expected argument, space separated
# 
# Author: fenouil
###############################################################################

# This functions uses the commandArgs to retrieve arguments to Rscript function and tries to parse them
getArgs=function(expectedArgsDefinition)
{
	
	# Check that mandatory parameters in the 'expected arguments definition list' are all defined
	listMandatory=c("variableName","numeric","mandatory", "description") # default, postConversion 
	missingParams=lapply(lapply(expectedArgsDefinition, names), function(x) {return(listMandatory[!(listMandatory %in% x)])}) # returns all the required and missing parameters for each expected argument
	nbMissing=sapply(missingParams,length);
	if(!all(nbMissing==0)) # is there some parameters missing ?
	{
		stop(paste("\nIncorrect list of expected arguments, some required fields are lacking :\n", paste(mapply(function(nameExpectedArg, missingParam)
										{
											return(paste("Argument \"", nameExpectedArg,"\" missing : ", paste(missingParam, collapse=", "), sep=""))
										}, names(missingParams[nbMissing>0]), missingParams[nbMissing>0]),collapse="\n"), sep=""))	
	}
	
	# Check that the 'expected arguments' names are not duplicated (supposing that the developper specified simple names with eventual '|' separator, in case of complex regular expression, this test might not be able to see a redundancy)
	expectedArgsNames_splitted=unlist(strsplit(names(expectedArgsDefinition), "\\|"));
	if(any(duplicated(expectedArgsNames_splitted))) stop(paste("Wrong argument definition, one expected argument name is used more than once by the script. Developper should check the definition list names. Duplicated argument(s) : ",paste(expectedArgsNames_splitted[duplicated(expectedArgsNames_splitted)], collapse=", "),sep=""))
	
	
	# Check if numeric is TRUE, and, in this case override the postConversion parameter
	expectedArgsDefinition=lapply(expectedArgsDefinition,function(x)
			{
				if(x$numeric) x$postConversion=as.numeric;
				return(x);
			})
	
	
	# retrieving arguments and values from command line
	receivedArgs = commandArgs(trailingOnly = TRUE)
	#receivedArgs = c("-i", "lalala", "-t", "4325")
	
	
	# Prepare the function writing instructions for exception catching
	expectedArgsInstructions=formatExpectedArgsInstructions(expectedArgsDefinition);
	printInstructions=function(){cat(expectedArgsInstructions); options(error=NULL);stop()};
	options(error=printInstructions);
	
	cat("\nReceived :\n  ")
	cat(receivedArgs,"", sep="\n  ")
	
	# Modifying expected arguments names in order to allow only an exact match in grep
	expectedArgsNames=paste("^", gsub("\\|", "$|^", names(expectedArgsDefinition)), "$", sep="");
	
	# Searching for expected arguments names in the received character vector
	argsIndexes=grep(paste(expectedArgsNames, collapse="|"), receivedArgs);
	
	
	# Check that all specified arguments have at least one value
	if(any(diff(argsIndexes)<2) | max(argsIndexes)==length(receivedArgs)) {stop(paste("At least one argument has no value", sep=""));}
	
	# Process each expected argument
	for(currentExpectedArg in names(expectedArgsDefinition))
	{
		
		print(currentExpectedArg)
		
		# Modifying params names in order to allow only an exact match in grep
		currentExpectedArgNames=paste("^", gsub("\\|", "$|^", currentExpectedArg), "$", sep="");
		
		currentArgIndex=grep(currentExpectedArgNames, receivedArgs);
		
		# Checks that an argument is not defined more than once, that mandatory arguments are provided
		if(length(currentArgIndex) > 1) {stop(paste("Duplicated argument definition (", currentExpectedArg, ")", sep=""));}
		if(expectedArgsDefinition[[currentExpectedArg]]$mandatory & length(currentArgIndex)==0) {stop(paste("Missing mandatory argument (", currentExpectedArg, ")", sep=""));} # Replaces alternative strategy
		
		################### All these comments correspond to an alternative strategy to handle mandatory arguments and default values
#		if(expectedArgsDefinition[[currentExpectedArg]]$mandatory & length(currentArgIndex)==0 & is.null(expectedArgsDefinition[[currentExpectedArg]]$default)) {stop(paste("Missing mandatory argument with no default value (", currentExpectedArg, ")", sep=""));}
		
		
		# In case the parameter is mandatory, NOT PROVIDED but has a default value, assign it
#		if(expectedArgsDefinition[[currentExpectedArg]]$mandatory & length(currentArgIndex)==0 & length(expectedArgsDefinition[[currentExpectedArg]]$default)!=0)
#		{
#			print("DEFAULT")
#			currentArgValues=expectedArgsDefinition[[currentExpectedArg]]$default;
		
		# Handle the parameter type
#			if(expectedArgsDefinition[[currentExpectedArg]]$numeric) currentArgValues=tryCatch(expr=as.numeric(currentArgValues), warning=function(errMessage){stop(paste(currentExpectedArg,"must be numeric", sep=" "))});
		
		# Put the variable and its value(s) in the global environment
#			assign(x=expectedArgsDefinition[[currentExpectedArg]]$variableName, value=currentArgValues, pos=globalenv());
#		}
		################### 
		
		## Get the provided or default values
		#currentArgValues=vector();
		currentArgValues=NULL;
		
		# Process parameter if VALUE PROVIDED
		if(length(currentArgIndex)!=0)# Actually '==1' should be equivalent because multiple definition should not be allowed by previous tests
		{
			# Detect the position of the next argument in the command line (in order to treat as current param values the stuff in between)
			# if there is no next argument, get all values until the end of the command line
			superiorArgIndexes=argsIndexes[which(argsIndexes>currentArgIndex)];
			valuesLastIndex=ifelse(length(superiorArgIndexes)>0,min(superiorArgIndexes)-1,length(receivedArgs));
			currentArgValues=receivedArgs[(currentArgIndex+1):valuesLastIndex];
		}
		else if(!is.null(expectedArgsDefinition[[currentExpectedArg]]$default)) # test for an eventuel default value if VALUE NOT PROVIDED
		{
			currentArgValues=expectedArgsDefinition[[currentExpectedArg]]$default;
		}
		
		
		# If a value has been assigned (coming indiferently from user or default value)
		# Treat, convert and assign it
		#if(length(currentArgValues)>0)
		if(!is.null(currentArgValues))
		{
			# - Handle R reserved values (NA, NULL)
			currentArgValuesIndexNA=(currentArgValues=="NA")
			currentArgValues[currentArgValuesIndexNA]=NA;
			
			
			if(any(currentArgValues=="NULL", na.rm=TRUE)) # in case of a NULL value, ignore all the other values in the vector (see documentation)
			{
				currentArgValues=NULL;
			}
			else if(!is.null(expectedArgsDefinition[[currentExpectedArg]]$postConversion)) # Apply eventual post conversion function
			{
				if(is.function(expectedArgsDefinition[[currentExpectedArg]]$postConversion))
				{
					currentArgValues=tryCatch(expr=expectedArgsDefinition[[currentExpectedArg]]$postConversion(currentArgValues), warning=function(errMessage){stop(paste(currentExpectedArg,"could not be converted to final type", sep=" "))});
					print("conversion")
					print(typeof(currentArgValues))
				}
				else
				{
					stop(paste("The postConversion parameter is not a declared function for argument", currentExpectedArg, sep=" "));
				}
			}
			
			# Put the variable and its value(s) in the global environment
			assign(x=expectedArgsDefinition[[currentExpectedArg]]$variableName, value=currentArgValues, pos=globalenv());
		}
		
	} # for(currentExpectedArg)
	
	# Reinitialize the error manager to default
	options(error=NULL);
}


# Define a synonym for compatibility
getParams=getArgs;