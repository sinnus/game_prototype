package org.my.jsonrpc
{
	public class Fault extends Error
	{
		public function Fault(message:String="", id:int=0)
		{
			super(message, id);
		}
	}
}