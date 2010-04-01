package org.my.jsonrpc
{
	import flash.events.Event;

	public class ResultEvent extends Event
	{
		public static const RESULT:String = "result";
		private var _result:Object;
		
		public function ResultEvent(type:String, result:Object)
		{
			super(type);
			_result = result;
		}
		
		public function get result():Object
		{
			return _result;
		}
		
	}
}