package org.my.jsonrpc
{
    import com.adobe.serialization.json.JSON;

    import flash.events.Event;
    import flash.events.IOErrorEvent;
    import flash.events.SecurityErrorEvent;
    import flash.net.URLLoader;
    import flash.net.URLRequest;

    public class JSONHTTPResponder
    {
        private var _loader:URLLoader;
        private var _resultCallback:Function;
        private var _faultCallback:Function;

        public function JSONHTTPResponder()
        {
            _loader = new URLLoader();
            _loader.addEventListener(Event.COMPLETE, handleComplete);
            _loader.addEventListener(IOErrorEvent.IO_ERROR, handleIOError);
            _loader.addEventListener(SecurityErrorEvent.SECURITY_ERROR, handleSecurityError);
        }

        public function load(urlRequest:URLRequest):void
        {
            _loader.load(urlRequest);
        }

        private function handleSecurityError(event:SecurityErrorEvent):void
        {
            var fault:Fault = new Fault(event.text);
            var faultEvent:FaultEvent = new FaultEvent(FaultEvent.FAULT, fault);
            doFaultCallback(faultEvent);
        }

        // TODO: Add fault codes
        private function handleIOError(event:IOErrorEvent):void
        {
            var fault:Fault = new Fault(event.text);
            var faultEvent:FaultEvent = new FaultEvent(FaultEvent.FAULT, fault);
            doFaultCallback(faultEvent);
        }

        private function handleComplete(event:Event):void
        {
            var loader:URLLoader = URLLoader(event.target);
            var rawData:String = String(loader.data);
            var jsonResult:Object = JSON.decode(rawData);
            if (jsonResult.error == null)
            {
                var resultEvent:ResultEvent = new ResultEvent(ResultEvent.RESULT, jsonResult.result);
                doResultCallback(resultEvent);
            }
            else
            {
                var fault:Fault = new Fault(jsonResult.error);
                var faultEvent:FaultEvent = new FaultEvent(FaultEvent.FAULT, fault);
                doFaultCallback(faultEvent);
            }
        }

        private function doFaultCallback(event:FaultEvent):void
        {
            if (_faultCallback != null)
            {
                _faultCallback(event);
            };
        }

        private function doResultCallback(event:ResultEvent):void
        {
            if (_resultCallback != null)
            {
                _resultCallback(event);
            };
        }

        public function set resultCallback(callback:Function):void
        {
            _resultCallback = callback;
        }

        public function set faultCallback(callback:Function):void
        {
            _faultCallback = callback;
        }

    }
}