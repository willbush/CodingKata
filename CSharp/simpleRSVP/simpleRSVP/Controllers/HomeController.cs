using simpleRSVP.Models;
using System.Web.Mvc;

namespace simpleRSVP.Controllers {

	public class HomeController : Controller {

		// GET: Home
		public ViewResult Index() {
			return View();
		}

		[HttpGet]
		public ViewResult RsvpForm() {
			return View();
		}

		[HttpPost]
		public ViewResult RsvpForm(GuestResponse response) {
			if (ModelState.IsValid) {
				return View("Thanks", response);
			} else {
				return View();
			}
		}
	}
}