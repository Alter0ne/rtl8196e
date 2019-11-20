/* valadynamicmethod.vala
 *
 * Copyright (C) 2007-2008  Jürg Billeter
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 *
 * Author:
 * 	Jürg Billeter <j@bitron.ch>
 */

using GLib;

/**
 * Represents a late bound method.
 */
public class Vala.DynamicMethod : Method {
	public DataType dynamic_type { get; set; }

	public MethodCall invocation { get; set; }

	public DynamicMethod (DataType dynamic_type, string name, DataType return_type, SourceReference? source_reference = null, Comment? comment = null) {
		base (name, return_type, source_reference, comment);
		this.dynamic_type = dynamic_type;
	}

	public override bool check (CodeContext context) {
		return true;
	}
}
