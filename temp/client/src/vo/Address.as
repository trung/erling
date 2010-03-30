package vo
{
	[RemoteClass(alias="org.mdkt.entity.Address")]
	[Bindable]
	public class Address extends BaseVO
	{
		public var street:String;
		public var postcode:int;
		
		public function Address()
		{
			
		}

	}
}