public class Class {
	public delegate void Foo ();

	public void foo () {
		assert_not_reached ();
	}

	public void test (Foo foo) {
		Foo func = () => { foo (); };
		func ();
	}


	void active_scope () {
		foreach (var bar in new string[] {}) {
		}

		var bar = "bar";
	}
}

void main () {
	var cl = new Class ();
	cl.test (() => { });
}
