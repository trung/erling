package vo
{
	[RemoteClass(alias="org.mdkt.entity.Person")]
	[Bindable]
	public class Person extends BaseVO
	{
		public var name:String;
		public var age:int = 10;
		public var height:Number = 2.0;
		public var address:Address;
		public var hobbies:Object;
		
		public function Person()
		{
			
		}

	}
}