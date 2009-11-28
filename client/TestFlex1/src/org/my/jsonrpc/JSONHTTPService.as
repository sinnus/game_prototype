package org.my.jsonrpc
{
    import com.adobe.serialization.json.JSON;

    import flash.events.EventDispatcher;
    import flash.net.URLRequest;
    import flash.net.URLRequestMethod;

    public class JSONHTTPService extends EventDispatcher
    {
        public function JSONHTTPService()
        {
            super();
        }

        public function callService(method:String, args:Array,
                                    resultHandler:Function = null,  faultHandler:Function = null):void
        {
            var req:URLRequest = new URLRequest("http://localhost:8000/json");
            req.method = URLRequestMethod.POST;
            var message:String = JSON.encode(args);
            req.data = '{"method": "' + method + '", "id": null, "params": [' + message + ']}';
            var jsonResponder:JSONHTTPResponder = new JSONHTTPResponder();
            jsonResponder.resultCallback = resultHandler;
            jsonResponder.faultCallback = faultHandler;
            jsonResponder.load(req);
        }
    }
}