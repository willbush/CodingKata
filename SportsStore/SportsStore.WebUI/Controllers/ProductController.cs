﻿using SportsStore.Domain.Abstract;
using SportsStore.WebUI.Models;
using System.Linq;
using System.Web.Mvc;

namespace SportsStore.WebUI.Controllers {

	public class ProductController : Controller {
		private IProductsRepository repository;
		public int pageSize = 4;

		public ProductController(IProductsRepository productRepository) {
			this.repository = productRepository;
		}

		public ViewResult List(int page = 1) {
			ProductsListViewModel model = new ProductsListViewModel {
				Products = repository.Products
				.OrderBy(p => p.ProductID)
				.Skip((page - 1) * pageSize)
				.Take(pageSize),

				PagingInfo = new PagingInfo {
					CurrentPage = page,
					ItemsPerPage = pageSize,
					TotalItems = repository.Products.Count()
				}
			};
			return View(model);
		}
	}
}