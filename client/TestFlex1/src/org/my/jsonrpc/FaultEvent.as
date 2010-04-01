package org.my.jsonrpc
{
    import flash.events.Event;

    public class FaultEvent extends Event
    {
        public static const FAULT:String = "fault";
        private var _fault:Fault;

        public function FaultEvent(type:String, fault:Fault)
        {
            super(type);
            _fault = fault;
        }

        public function get fault():Fault
        {
            return _fault;
        }
    }
}