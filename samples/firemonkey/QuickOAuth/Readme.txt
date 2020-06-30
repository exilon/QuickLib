This demo demonstrates how to use Quick.OAuth class to create your own OAuth clients.

It creates a class to access GMail account. Accessing other services is similar to this approach.

Instructions
	1. You need to register your application with Google to allow it to use the APIs. This will provide you with a ClientID and SecretID
	
	2. Create a subclass of TOAuthBase
	
	3. Once you get the IDs, you can pass them to the TOAuth subclass in the constructor. Keep the IDs safely stored in your application. You, also, pass the callback URL to the CallbackURL property
	
	4. In the subclass, you need to implement three protected methods. Each method returns the URL for specific calls to API. 
	
	The return URL starts with the REST method separated by a space. For example, a GET request to www.google.com/oauth will be returned like this:
	
		CreateAuthorizationRequest --> GET www.google.com/oauth
		
	The three methods are:
		* CreateAuthorizationRequest: to customise the initial call to generate the authorisation code
	
		* CreateAuthToAccessRequest: to customise the call to convert the authorisation code to access token
		
		* CreateRefreshRequest: to refresh the access token
		
	5. Each API uses different tags to pass the tokens back to the applications. For example, google returns this JSON:
	
		{
		  "access_token": "123", 
		  "scope": "https://mail.google.com/", 
		  "token_type": "Bearer", 
		  "expires_in": 3599, 
		  "refresh_token":"ABC"
		}
		
		You need to tell TOAuth which tags to use to extract the information. Use the AccessTokenParam, ExpirationParam, RefreshTokenParam properties for this. In the above example, it would be:
			  
				AccessTokenParam:='access_token';
				ExpirationParam:='expires_in';
				RefreshTokenParam:='refresh_token';
		
	6. You can save/load the token via the OnSaveToken and OnLoadToken properties (events)
	
	7. The first time you want to access the service, you call Authorize
	
	8. After this, the access token is stored in the class and you can access it via the AccessToken property
	
	9. Every time you retrieve the AccessToken, TOAuth checks if it has expired. If it has, it refreshes it and passes the new one

	10. You can save all the tokens to a persistent medium but you only need to store the refresh token to avoid the whole authorisation process in the browser. Make sure you store it safely by using strong encryption 
	
	