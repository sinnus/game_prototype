package org.my.jsonrpc
{
    import com.adobe.serialization.json.JSON;

    import flash.events.EventDispatcher;
    import flash.net.URLRequest;
    import flash.net.URLRequestMethod;

    import mx.utils.UIDUtil;

    public class JSONHTTPService extends EventDispatcher
    {
        private var _url:String;

        public function JSONHTTPService(url:String)
        {
            super();
            _url = url;
        }

        public function callService(method:String, args:Object,
                                    resultHandler:Function = null,  faultHandler:Function = null):void
        {
            var req:URLRequest = new URLRequest(_url);
            req.method = URLRequestMethod.POST;
            var message:String = JSON.encode(args);
            req.data = '{"method": "' + method + '", "id": null, "params": ' + message + '}';
            var jsonResponder:JSONHTTPResponder = new JSONHTTPResponder();
            jsonResponder.resultCallback = resultHandler;
            jsonResponder.faultCallback = faultHandler;
            jsonResponder.load(req);
        }

        public function get url():String
        {
            return _url;
        }

    }
}