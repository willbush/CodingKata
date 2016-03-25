using SportsStore.Domain.Entities;
using System.Data.Entity;

namespace SportsStore.Domain.Concrete {

	internal class EFDbContext : DbContext {

		public DbSet<Product> Products { get; set; }
	}
}