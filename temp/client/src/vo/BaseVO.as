package vo
{
	[RemoteClass(alias="org.mdkt.entity.BaseEntity")]
	[Bindable]
	public class BaseVO
	{
		public var createdBy:String;
		public var createdOn:Date;
		
		public function BaseVO()
		{
			
		}

	}
}