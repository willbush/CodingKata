using simpleRSVP.Models;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;

namespace simpleRSVP.Controllers {

	public class HomeController : Controller {

		// GET: Home
		public ViewResult Index() {
			int hour = DateTime.Now.Hour;
			ViewBag.Greeting = hour < 12 ? "Good Morning" : "Good Afternoon";
			return View();
		}

		[HttpGet]
		public ViewResult RsvpForm() {
			return View();
		}

		[HttpPost]
		public ViewResult RsvpForm(GuestResponse response) {
			if (ModelState.IsValid) {
				//TODO: Email response to the party organizer.
				return View("Thanks", response);
			} else {
				return View();
			}
		}
	}
}