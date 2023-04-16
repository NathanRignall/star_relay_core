with Ada.Calendar;
with Ada.Streams;
with Ada.Strings.Unbounded;

with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Server.Push;

package WS_CB is

   WS : AWS.Server.HTTP;

   function Service (Request : AWS.Status.Data) return AWS.Response.Data;

end WS_CB;