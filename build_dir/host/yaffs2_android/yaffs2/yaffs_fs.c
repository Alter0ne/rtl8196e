/*
 * YAFFS: Yet another FFS. A NAND-flash specific file system.
 * yaffs_fs.c
 *
 * Copyright (C) 2002 Aleph One Ltd.
 *   for Toby Churchill Ltd and Brightstar Engineering
 *
 * Created by Charles Manning <charles@aleph1.co.uk>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 * This is the file system front-end to YAFFS that hooks it up to
 * the VFS.
 *
 * Special notes: 
 * >> 2.4: sb->u.generic_sbp points to the yaffs_Device associated with
 *         this superblock
 * >> 2.6: sb->s_fs_info  points to the yaffs_Device associated with this
 *         superblock
 * >> inode->u.generic_ip points to the associated yaffs_Object.
 *
 * Acknowledgements:
 * * Luc van OostenRyck for numerous patches.
 * * Nick Bane for numerous patches.
 * * Nick Bane for 2.5/2.6 integration.
 * * Andras Toth for mknod rdev issue.
 * * Michael Fischer for finding the problem with inode inconsistency.
 * * Some code bodily lifted from JFFS2.
 */

const char *yaffs_fs_c_version =
    "$Id: yaffs_fs.c,v 1.53 2006/10/03 10:13:03 charles Exp $";
extern const char *yaffs_guts_c_version;

#include <linux/config.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/version.h>
#include <linux/slab.h>
#include <linux/init.h>
#include <linux/list.h>
#include <linux/fs.h>
#include <linux/proc_fs.h>
#include <linux/smp_lock.h>
#include <linux/pagemap.h>
#include <linux/mtd/mtd.h>
#include <linux/interrupt.h>
#include <linux/string.h>
#include <linux/ctype.h>

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))

#include <linux/statfs.h>	/* Added NCB 15-8-2003 */
#include <asm/statfs.h>
#define UnlockPage(p) unlock_page(p)
#define Page_Uptodate(page)	test_bit(PG_uptodate, &(page)->flags)

/* FIXME: use sb->s_id instead ? */
#define yaffs_devname(sb, buf)	bdevname(sb->s_bdev, buf)

#else

#include <linux/locks.h>
#define	BDEVNAME_SIZE		0
#define	yaffs_devname(sb, buf)	kdevname(sb->s_dev)

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2,5,0))
/* added NCB 26/5/2006 for 2.4.25-vrs2-tcl1 kernel */
#define __user
#endif

#endif

#include <asm/uaccess.h>

#include "yportenv.h"
#include "yaffs_guts.h"

unsigned yaffs_traceMask = YAFFS_TRACE_ALWAYS | 
			   YAFFS_TRACE_BAD_BLOCKS | 
			   YAFFS_TRACE_CHECKPOINT
			   /* | 0xFFFFFFFF */; 

#include <linux/mtd/mtd.h>
#include "yaffs_mtdif.h"
#include "yaffs_mtdif2.h"

/*#define T(x) printk x */

#define yaffs_InodeToObject(iptr) ((yaffs_Object *)((iptr)->u.generic_ip))
#define yaffs_DentryToObject(dptr) yaffs_InodeToObject((dptr)->d_inode)

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))
#define yaffs_SuperToDevice(sb)	((yaffs_Device *)sb->s_fs_info)
#else
#define yaffs_SuperToDevice(sb)	((yaffs_Device *)sb->u.generic_sbp)
#endif

static void yaffs_put_super(struct super_block *sb);

static ssize_t yaffs_file_write(struct file *f, const char *buf, size_t n,
				loff_t * pos);

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,17))
static int yaffs_file_flush(struct file *file, fl_owner_t id);
#else
static int yaffs_file_flush(struct file *file);
#endif

static int yaffs_sync_object(struct file *file, struct dentry *dentry,
			     int datasync);

static int yaffs_readdir(struct file *f, void *dirent, filldir_t filldir);

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))
static int yaffs_create(struct inode *dir, struct dentry *dentry, int mode,
			struct nameidata *n);
static struct dentry *yaffs_lookup(struct inode *dir, struct dentry *dentry,
				   struct nameidata *n);
#else
static int yaffs_create(struct inode *dir, struct dentry *dentry, int mode);
static struct dentry *yaffs_lookup(struct inode *dir, struct dentry *dentry);
#endif
static int yaffs_link(struct dentry *old_dentry, struct inode *dir,
		      struct dentry *dentry);
static int yaffs_unlink(struct inode *dir, struct dentry *dentry);
static int yaffs_symlink(struct inode *dir, struct dentry *dentry,
			 const char *symname);
static int yaffs_mkdir(struct inode *dir, struct dentry *dentry, int mode);

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))
static int yaffs_mknod(struct inode *dir, struct dentry *dentry, int mode,
		       dev_t dev);
#else
static int yaffs_mknod(struct inode *dir, struct dentry *dentry, int mode,
		       int dev);
#endif
static int yaffs_rename(struct inode *old_dir, struct dentry *old_dentry,
			struct inode *new_dir, struct dentry *new_dentry);
static int yaffs_setattr(struct dentry *dentry, struct iattr *attr);

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,17))
static int yaffs_sync_fs(struct super_block *sb, int wait);
static void yaffs_write_super(struct super_block *sb);
#else
static int yaffs_sync_fs(struct super_block *sb);
static int yaffs_write_super(struct super_block *sb);
#endif

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,17))
static int yaffs_statfs(struct dentry *dentry, struct kstatfs *buf);
#elif (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))
static int yaffs_statfs(struct super_block *sb, struct kstatfs *buf);
#else
static int yaffs_statfs(struct super_block *sb, struct statfs *buf);
#endif
static void yaffs_read_inode(struct inode *inode);

static void yaffs_put_inode(struct inode *inode);
static void yaffs_delete_inode(struct inode *);
static void yaffs_clear_inode(struct inode *);

static int yaffs_readpage(struct file *file, struct page *page);
#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))
static int yaffs_writepage(struct page *page, struct writeback_control *wbc);
#else
static int yaffs_writepage(struct page *page);
#endif
static int yaffs_prepare_write(struct file *f, struct page *pg,
			       unsigned offset, unsigned to);
static int yaffs_commit_write(struct file *f, struct page *pg, unsigned offset,
			      unsigned to);

static int yaffs_readlink(struct dentry *dentry, char __user * buffer,
			  int buflen);
#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,13))
static void *yaffs_follow_link(struct dentry *dentry, struct nameidata *nd);
#else
static int yaffs_follow_link(struct dentry *dentry, struct nameidata *nd);
#endif

static struct address_space_operations yaffs_file_address_operations = {
	.readpage = yaffs_readpage,
	.writepage = yaffs_writepage,
	.prepare_write = yaffs_prepare_write,
	.commit_write = yaffs_commit_write,
};

static struct file_operations yaffs_file_operations = {
	.read = generic_file_read,
	.write = generic_file_write,
	.mmap = generic_file_mmap,
	.flush = yaffs_file_flush,
	.fsync = yaffs_sync_object,
#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))
	.sendfile = generic_file_sendfile,
#endif

};

static struct inode_operations yaffs_file_inode_operations = {
	.setattr = yaffs_setattr,
};

static struct inode_operations yaffs_symlink_inode_operations = {
	.readlink = yaffs_readlink,
	.follow_link = yaffs_follow_link,
	.setattr = yaffs_setattr,
};

static struct inode_operations yaffs_dir_inode_operations = {
	.create = yaffs_create,
	.lookup = yaffs_lookup,
	.link = yaffs_link,
	.unlink = yaffs_unlink,
	.symlink = yaffs_symlink,
	.mkdir = yaffs_mkdir,
	.rmdir = yaffs_unlink,
	.mknod = yaffs_mknod,
	.rename = yaffs_rename,
	.setattr = yaffs_setattr,
};

static struct file_operations yaffs_dir_operations = {
	.read = generic_read_dir,
	.readdir = yaffs_readdir,
	.fsync = yaffs_sync_object,
};

static struct super_operations yaffs_super_ops = {
	.statfs = yaffs_statfs,
	.read_inode = yaffs_read_inode,
	.put_inode = yaffs_put_inode,
	.put_super = yaffs_put_super,
	.delete_inode = yaffs_delete_inode,
	.clear_inode = yaffs_clear_inode,
	.sync_fs = yaffs_sync_fs,
	.write_super = yaffs_write_super,
};

static void yaffs_GrossLock(yaffs_Device * dev)
{
	T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs locking\n"));

	down(&dev->grossLock);
}

static void yaffs_GrossUnlock(yaffs_Device * dev)
{
	T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs unlocking\n"));
	up(&dev->grossLock);

}

static int yaffs_readlink(struct dentry *dentry, char __user * buffer,
			  int buflen)
{
	unsigned char *alias;
	int ret;

	yaffs_Device *dev = yaffs_DentryToObject(dentry)->myDev;

	yaffs_GrossLock(dev);

	alias = yaffs_GetSymlinkAlias(yaffs_DentryToObject(dentry));

	yaffs_GrossUnlock(dev);

	if (!alias)
		return -ENOMEM;

	ret = vfs_readlink(dentry, buffer, buflen, alias);
	kfree(alias);
	return ret;
}

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,13))
static void *yaffs_follow_link(struct dentry *dentry, struct nameidata *nd)
#else
static int yaffs_follow_link(struct dentry *dentry, struct nameidata *nd)
#endif
{
	unsigned char *alias;
	int ret;
	yaffs_Device *dev = yaffs_DentryToObject(dentry)->myDev;

	yaffs_GrossLock(dev);

	alias = yaffs_GetSymlinkAlias(yaffs_DentryToObject(dentry));

	yaffs_GrossUnlock(dev);

	if (!alias)
        {
		ret = -ENOMEM;
		goto out;
        }

	ret = vfs_follow_link(nd, alias);
	kfree(alias);
out:
#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,13))
	return ERR_PTR (ret);
#else
	return ret;
#endif
}

struct inode *yaffs_get_inode(struct super_block *sb, int mode, int dev,
			      yaffs_Object * obj);

/*
 * Lookup is used to find objects in the fs
 */
#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))

static struct dentry *yaffs_lookup(struct inode *dir, struct dentry *dentry,
				   struct nameidata *n)
#else
static struct dentry *yaffs_lookup(struct inode *dir, struct dentry *dentry)
#endif
{
	yaffs_Object *obj;
	struct inode *inode = NULL;	/* NCB 2.5/2.6 needs NULL here */

	yaffs_Device *dev = yaffs_InodeToObject(dir)->myDev;

	yaffs_GrossLock(dev);

	T(YAFFS_TRACE_OS,
	  (KERN_DEBUG "yaffs_lookup for %d:%s\n",
	   yaffs_InodeToObject(dir)->objectId, dentry->d_name.name));

	obj =
	    yaffs_FindObjectByName(yaffs_InodeToObject(dir),
				   dentry->d_name.name);

	obj = yaffs_GetEquivalentObject(obj);	/* in case it was a hardlink */
	
	/* Can't hold gross lock when calling yaffs_get_inode() */
	yaffs_GrossUnlock(dev);

	if (obj) {
		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG "yaffs_lookup found %d\n", obj->objectId));

		inode = yaffs_get_inode(dir->i_sb, obj->yst_mode, 0, obj);

		if (inode) {
			T(YAFFS_TRACE_OS,
			  (KERN_DEBUG "yaffs_loookup dentry \n"));
/* #if 0 asserted by NCB for 2.5/6 compatability - falls through to
 * d_add even if NULL inode */
#if 0
			/*dget(dentry); // try to solve directory bug */
			d_add(dentry, inode);

			/* return dentry; */
			return NULL;
#endif
		}

	} else {
		T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_lookup not found\n"));

	}

/* added NCB for 2.5/6 compatability - forces add even if inode is
 * NULL which creates dentry hash */
	d_add(dentry, inode);

	return NULL;
	/*      return (ERR_PTR(-EIO)); */

}

/* For now put inode is just for debugging
 * Put inode is called when the inode **structure** is put.
 */
static void yaffs_put_inode(struct inode *inode)
{
	T(YAFFS_TRACE_OS,
	  ("yaffs_put_inode: ino %d, count %d\n", (int)inode->i_ino,
	   atomic_read(&inode->i_count)));

}

/* clear is called to tell the fs to release any per-inode data it holds */
static void yaffs_clear_inode(struct inode *inode)
{
	yaffs_Object *obj;
	yaffs_Device *dev;

	obj = yaffs_InodeToObject(inode);

	T(YAFFS_TRACE_OS,
	  ("yaffs_clear_inode: ino %d, count %d %s\n", (int)inode->i_ino,
	   atomic_read(&inode->i_count),
	   obj ? "object exists" : "null object"));

	if (obj) {
		dev = obj->myDev;
		yaffs_GrossLock(dev);

		/* Clear the association between the inode and
		 * the yaffs_Object.
		 */
		obj->myInode = NULL;
		inode->u.generic_ip = NULL;

		/* If the object freeing was deferred, then the real
		 * free happens now.
		 * This should fix the inode inconsistency problem.
		 */

		yaffs_HandleDeferedFree(obj);

		yaffs_GrossUnlock(dev);
	}

}

/* delete is called when the link count is zero and the inode
 * is put (ie. nobody wants to know about it anymore, time to
 * delete the file).
 * NB Must call clear_inode()
 */
static void yaffs_delete_inode(struct inode *inode)
{
	yaffs_Object *obj = yaffs_InodeToObject(inode);
	yaffs_Device *dev;

	T(YAFFS_TRACE_OS,
	  ("yaffs_delete_inode: ino %d, count %d %s\n", (int)inode->i_ino,
	   atomic_read(&inode->i_count),
	   obj ? "object exists" : "null object"));

	if (obj) {
		dev = obj->myDev;
		yaffs_GrossLock(dev);
		yaffs_DeleteFile(obj);
		yaffs_GrossUnlock(dev);
	}
#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,13))
        truncate_inode_pages (&inode->i_data, 0);
#endif
	clear_inode(inode);
}

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,17))
static int yaffs_file_flush(struct file *file, fl_owner_t id)
#else
static int yaffs_file_flush(struct file *file)
#endif
{
	yaffs_Object *obj = yaffs_DentryToObject(file->f_dentry);

	yaffs_Device *dev = obj->myDev;

	T(YAFFS_TRACE_OS,
	  (KERN_DEBUG "yaffs_file_flush object %d (%s)\n", obj->objectId,
	   obj->dirty ? "dirty" : "clean"));

	yaffs_GrossLock(dev);

	yaffs_FlushFile(obj, 1);

	yaffs_GrossUnlock(dev);

	return 0;
}

static int yaffs_readpage_nolock(struct file *f, struct page *pg)
{
	/* Lifted from jffs2 */

	yaffs_Object *obj;
	unsigned char *pg_buf;
	int ret;

	yaffs_Device *dev;

	T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_readpage at %08x, size %08x\n",
			   (unsigned)(pg->index << PAGE_CACHE_SHIFT),
			   (unsigned)PAGE_CACHE_SIZE));

	obj = yaffs_DentryToObject(f->f_dentry);

	dev = obj->myDev;

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))
	BUG_ON(!PageLocked(pg));
#else
	if (!PageLocked(pg))
		PAGE_BUG(pg);
#endif

	pg_buf = kmap(pg);
	/* FIXME: Can kmap fail? */

	yaffs_GrossLock(dev);

	ret =
	    yaffs_ReadDataFromFile(obj, pg_buf, pg->index << PAGE_CACHE_SHIFT,
				   PAGE_CACHE_SIZE);

	yaffs_GrossUnlock(dev);

	if (ret >= 0)
		ret = 0;

	if (ret) {
		ClearPageUptodate(pg);
		SetPageError(pg);
	} else {
		SetPageUptodate(pg);
		ClearPageError(pg);
	}

	flush_dcache_page(pg);
	kunmap(pg);

	T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_readpage done\n"));
	return ret;
}

static int yaffs_readpage_unlock(struct file *f, struct page *pg)
{
	int ret = yaffs_readpage_nolock(f, pg);
	UnlockPage(pg);
	return ret;
}

static int yaffs_readpage(struct file *f, struct page *pg)
{
	return yaffs_readpage_unlock(f, pg);
}

/* writepage inspired by/stolen from smbfs */

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))
static int yaffs_writepage(struct page *page, struct writeback_control *wbc)
#else
static int yaffs_writepage(struct page *page)
#endif
{
	struct address_space *mapping = page->mapping;
	loff_t offset = (loff_t) page->index << PAGE_CACHE_SHIFT;
	struct inode *inode;
	unsigned long end_index;
	char *buffer;
	yaffs_Object *obj;
	int nWritten = 0;
	unsigned nBytes;

	if (!mapping)
		BUG();
	inode = mapping->host;
	if (!inode)
		BUG();

	if (offset > inode->i_size) {
		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG
		   "yaffs_writepage at %08x, inode size = %08x!!!\n",
		   (unsigned)(page->index << PAGE_CACHE_SHIFT),
		   (unsigned)inode->i_size));
		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG "                -> don't care!!\n"));
		unlock_page(page);
		return 0;
	}

	end_index = inode->i_size >> PAGE_CACHE_SHIFT;

	/* easy case */
	if (page->index < end_index) {
		nBytes = PAGE_CACHE_SIZE;
	} else {
		nBytes = inode->i_size & (PAGE_CACHE_SIZE - 1);
	}

	get_page(page);

	buffer = kmap(page);

	obj = yaffs_InodeToObject(inode);
	yaffs_GrossLock(obj->myDev);

	T(YAFFS_TRACE_OS,
	  (KERN_DEBUG "yaffs_writepage at %08x, size %08x\n",
	   (unsigned)(page->index << PAGE_CACHE_SHIFT), nBytes));
	T(YAFFS_TRACE_OS,
	  (KERN_DEBUG "writepag0: obj = %05x, ino = %05x\n",
	   (int)obj->variant.fileVariant.fileSize, (int)inode->i_size));

	nWritten =
	    yaffs_WriteDataToFile(obj, buffer, page->index << PAGE_CACHE_SHIFT,
				  nBytes, 0);

	T(YAFFS_TRACE_OS,
	  (KERN_DEBUG "writepag1: obj = %05x, ino = %05x\n",
	   (int)obj->variant.fileVariant.fileSize, (int)inode->i_size));

	yaffs_GrossUnlock(obj->myDev);

	kunmap(page);
	SetPageUptodate(page);
	UnlockPage(page);
	put_page(page);

	return (nWritten == nBytes) ? 0 : -ENOSPC;
}

static int yaffs_prepare_write(struct file *f, struct page *pg,
			       unsigned offset, unsigned to)
{

	T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_prepair_write\n"));
	if (!Page_Uptodate(pg) && (offset || to < PAGE_CACHE_SIZE))
		return yaffs_readpage_nolock(f, pg);

	return 0;

}

static int yaffs_commit_write(struct file *f, struct page *pg, unsigned offset,
			      unsigned to)
{

	void *addr = page_address(pg) + offset;
	loff_t pos = (((loff_t) pg->index) << PAGE_CACHE_SHIFT) + offset;
	int nBytes = to - offset;
	int nWritten;

	unsigned spos = pos;
	unsigned saddr = (unsigned)addr;

	T(YAFFS_TRACE_OS,
	  (KERN_DEBUG "yaffs_commit_write addr %x pos %x nBytes %d\n", saddr,
	   spos, nBytes));

	nWritten = yaffs_file_write(f, addr, nBytes, &pos);

	if (nWritten != nBytes) {
		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG
		   "yaffs_commit_write not same size nWritten %d  nBytes %d\n",
		   nWritten, nBytes));
		SetPageError(pg);
		ClearPageUptodate(pg);
	} else {
		SetPageUptodate(pg);
	}

	T(YAFFS_TRACE_OS,
	  (KERN_DEBUG "yaffs_commit_write returning %d\n",
	   nWritten == nBytes ? 0 : nWritten));

	return nWritten == nBytes ? 0 : nWritten;

}

static void yaffs_FillInodeFromObject(struct inode *inode, yaffs_Object * obj)
{
	if (inode && obj) {


		/* Check mode against the variant type and attempt to repair if broken. */
 		__u32 mode = obj->yst_mode;
 		switch( obj->variantType ){
 		case YAFFS_OBJECT_TYPE_FILE :
 		        if( ! S_ISREG(mode) ){
 			        obj->yst_mode &= ~S_IFMT;
 			        obj->yst_mode |= S_IFREG;
 			}
 
 			break;
 		case YAFFS_OBJECT_TYPE_SYMLINK :
 		        if( ! S_ISLNK(mode) ){
 			        obj->yst_mode &= ~S_IFMT;
 				obj->yst_mode |= S_IFLNK;
 			}
 
 			break;
 		case YAFFS_OBJECT_TYPE_DIRECTORY :
 		        if( ! S_ISDIR(mode) ){
 			        obj->yst_mode &= ~S_IFMT;
 			        obj->yst_mode |= S_IFDIR;
 			}
 
 			break;
 		case YAFFS_OBJECT_TYPE_UNKNOWN :
 		case YAFFS_OBJECT_TYPE_HARDLINK :
 		case YAFFS_OBJECT_TYPE_SPECIAL :
 		default:
 		        /* TODO? */
 		        break;
 		}

		inode->i_ino = obj->objectId;
		inode->i_mode = obj->yst_mode;
		inode->i_uid = obj->yst_uid;
		inode->i_gid = obj->yst_gid;
		inode->i_blksize = inode->i_sb->s_blocksize;
#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))

		inode->i_rdev = old_decode_dev(obj->yst_rdev);
		inode->i_atime.tv_sec = (time_t) (obj->yst_atime);
		inode->i_atime.tv_nsec = 0;
		inode->i_mtime.tv_sec = (time_t) obj->yst_mtime;
		inode->i_mtime.tv_nsec = 0;
		inode->i_ctime.tv_sec = (time_t) obj->yst_ctime;
		inode->i_ctime.tv_nsec = 0;
#else
		inode->i_rdev = obj->yst_rdev;
		inode->i_atime = obj->yst_atime;
		inode->i_mtime = obj->yst_mtime;
		inode->i_ctime = obj->yst_ctime;
#endif
		inode->i_size = yaffs_GetObjectFileLength(obj);
		inode->i_blocks = (inode->i_size + 511) >> 9;

		inode->i_nlink = yaffs_GetObjectLinkCount(obj);

		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG
		   "yaffs_FillInode mode %x uid %d gid %d size %d count %d\n",
		   inode->i_mode, inode->i_uid, inode->i_gid,
		   (int)inode->i_size, atomic_read(&inode->i_count)));

		switch (obj->yst_mode & S_IFMT) {
		default:	/* fifo, device or socket */
#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))
			init_special_inode(inode, obj->yst_mode,
					   old_decode_dev(obj->yst_rdev));
#else
			init_special_inode(inode, obj->yst_mode,
					   (dev_t) (obj->yst_rdev));
#endif
			break;
		case S_IFREG:	/* file */
			inode->i_op = &yaffs_file_inode_operations;
			inode->i_fop = &yaffs_file_operations;
			inode->i_mapping->a_ops =
			    &yaffs_file_address_operations;
			break;
		case S_IFDIR:	/* directory */
			inode->i_op = &yaffs_dir_inode_operations;
			inode->i_fop = &yaffs_dir_operations;
			break;
		case S_IFLNK:	/* symlink */
			inode->i_op = &yaffs_symlink_inode_operations;
			break;
		}

		inode->u.generic_ip = obj;
		obj->myInode = inode;

	} else {
		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG "yaffs_FileInode invalid parameters\n"));
	}

}

struct inode *yaffs_get_inode(struct super_block *sb, int mode, int dev,
			      yaffs_Object * obj)
{
	struct inode *inode;

	if (!sb) {
		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG "yaffs_get_inode for NULL super_block!!\n"));
		return NULL;

	}

	if (!obj) {
		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG "yaffs_get_inode for NULL object!!\n"));
		return NULL;

	}

	T(YAFFS_TRACE_OS,
	  (KERN_DEBUG "yaffs_get_inode for object %d\n", obj->objectId));

	inode = iget(sb, obj->objectId);

	/* NB Side effect: iget calls back to yaffs_read_inode(). */
	/* iget also increments the inode's i_count */
	/* NB You can't be holding grossLock or deadlock will happen! */

	return inode;
}

static ssize_t yaffs_file_write(struct file *f, const char *buf, size_t n,
				loff_t * pos)
{
	yaffs_Object *obj;
	int nWritten, ipos;
	struct inode *inode;
	yaffs_Device *dev;

	obj = yaffs_DentryToObject(f->f_dentry);

	dev = obj->myDev;

	yaffs_GrossLock(dev);

	inode = f->f_dentry->d_inode;

	if (!S_ISBLK(inode->i_mode) && f->f_flags & O_APPEND) {
		ipos = inode->i_size;
	} else {
		ipos = *pos;
	}

	if (!obj) {
		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG "yaffs_file_write: hey obj is null!\n"));
	} else {
		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG
		   "yaffs_file_write about to write writing %d bytes"
		   "to object %d at %d\n",
		   n, obj->objectId, ipos));
	}

	nWritten = yaffs_WriteDataToFile(obj, buf, ipos, n, 0);

	T(YAFFS_TRACE_OS,
	  (KERN_DEBUG "yaffs_file_write writing %d bytes, %d written at %d\n",
	   n, nWritten, ipos));
	if (nWritten > 0) {
		ipos += nWritten;
		*pos = ipos;
		if (ipos > inode->i_size) {
			inode->i_size = ipos;
			inode->i_blocks = (ipos + 511) >> 9;

			T(YAFFS_TRACE_OS,
			  (KERN_DEBUG
			   "yaffs_file_write size updated to %d bytes, "
			   "%d blocks\n",
			   ipos, (int)(inode->i_blocks)));
		}

	}
	yaffs_GrossUnlock(dev);
	return nWritten == 0 ? -ENOSPC : nWritten;
}

static int yaffs_readdir(struct file *f, void *dirent, filldir_t filldir)
{
	yaffs_Object *obj;
	yaffs_Device *dev;
	struct inode *inode = f->f_dentry->d_inode;
	unsigned long offset, curoffs;
	struct list_head *i;
	yaffs_Object *l;

	char name[YAFFS_MAX_NAME_LENGTH + 1];

	obj = yaffs_DentryToObject(f->f_dentry);
	dev = obj->myDev;

	yaffs_GrossLock(dev);

	offset = f->f_pos;

	T(YAFFS_TRACE_OS, ("yaffs_readdir: starting at %d\n", (int)offset));

	if (offset == 0) {
		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG "yaffs_readdir: entry . ino %d \n",
		   (int)inode->i_ino));
		if (filldir(dirent, ".", 1, offset, inode->i_ino, DT_DIR)
		    < 0) {
			goto out;
		}
		offset++;
		f->f_pos++;
	}
	if (offset == 1) {
		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG "yaffs_readdir: entry .. ino %d \n",
		   (int)f->f_dentry->d_parent->d_inode->i_ino));
		if (filldir
		    (dirent, "..", 2, offset,
		     f->f_dentry->d_parent->d_inode->i_ino, DT_DIR) < 0) {
			goto out;
		}
		offset++;
		f->f_pos++;
	}

	curoffs = 1;

	/* If the directory has changed since the open or last call to
	   readdir, rewind to after the 2 canned entries. */

	if (f->f_version != inode->i_version) {
		offset = 2;
		f->f_pos = offset;
		f->f_version = inode->i_version;
	}

	list_for_each(i, &obj->variant.directoryVariant.children) {
		curoffs++;
		if (curoffs >= offset) {
			l = list_entry(i, yaffs_Object, siblings);

			yaffs_GetObjectName(l, name,
					    YAFFS_MAX_NAME_LENGTH + 1);
			T(YAFFS_TRACE_OS,
			  (KERN_DEBUG "yaffs_readdir: %s inode %d\n", name,
			   yaffs_GetObjectInode(l)));

			if (filldir(dirent,
				    name,
				    strlen(name),
				    offset,
				    yaffs_GetObjectInode(l),
				    yaffs_GetObjectType(l))
			    < 0) {
				goto up_and_out;
			}

			offset++;
			f->f_pos++;
		}
	}

      up_and_out:
      out:

	yaffs_GrossUnlock(dev);

	return 0;
}

/*
 * File creation. Allocate an inode, and we're done..
 */
#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))
static int yaffs_mknod(struct inode *dir, struct dentry *dentry, int mode,
		       dev_t rdev)
#else
static int yaffs_mknod(struct inode *dir, struct dentry *dentry, int mode,
		       int rdev)
#endif
{
	struct inode *inode;

	yaffs_Object *obj = NULL;
	yaffs_Device *dev;

	yaffs_Object *parent = yaffs_InodeToObject(dir);

	int error = -ENOSPC;
	uid_t uid = current->fsuid;
	gid_t gid = (dir->i_mode & S_ISGID) ? dir->i_gid : current->fsgid;
	
	if((dir->i_mode & S_ISGID) && S_ISDIR(mode))
		mode |= S_ISGID;

	if (parent) {
		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG "yaffs_mknod: parent object %d type %d\n",
		   parent->objectId, parent->variantType));
	} else {
		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG "yaffs_mknod: could not get parent object\n"));
		return -EPERM;
	}

	T(YAFFS_TRACE_OS, ("yaffs_mknod: making oject for %s, "
			   "mode %x dev %x\n",
			   dentry->d_name.name, mode, rdev));

	dev = parent->myDev;

	yaffs_GrossLock(dev);

	switch (mode & S_IFMT) {
	default:
		/* Special (socket, fifo, device...) */
		T(YAFFS_TRACE_OS, (KERN_DEBUG
				   "yaffs_mknod: making special\n"));
#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))
		obj =
		    yaffs_MknodSpecial(parent, dentry->d_name.name, mode, uid,
				       gid, old_encode_dev(rdev));
#else
		obj =
		    yaffs_MknodSpecial(parent, dentry->d_name.name, mode, uid,
				       gid, rdev);
#endif
		break;
	case S_IFREG:		/* file          */
		T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_mknod: making file\n"));
		obj =
		    yaffs_MknodFile(parent, dentry->d_name.name, mode, uid,
				    gid);
		break;
	case S_IFDIR:		/* directory */
		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG "yaffs_mknod: making directory\n"));
		obj =
		    yaffs_MknodDirectory(parent, dentry->d_name.name, mode,
					 uid, gid);
		break;
	case S_IFLNK:		/* symlink */
		T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_mknod: making file\n"));
		obj = NULL;	/* Do we ever get here? */
		break;
	}
	
	/* Can not call yaffs_get_inode() with gross lock held */
	yaffs_GrossUnlock(dev);

	if (obj) {
		inode = yaffs_get_inode(dir->i_sb, mode, rdev, obj);
		d_instantiate(dentry, inode);
		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG "yaffs_mknod created object %d count = %d\n",
		   obj->objectId, atomic_read(&inode->i_count)));
		error = 0;
	} else {
		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG "yaffs_mknod failed making object\n"));
		error = -ENOMEM;
	}

	return error;
}

static int yaffs_mkdir(struct inode *dir, struct dentry *dentry, int mode)
{
	int retVal;
	T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_mkdir\n"));
	retVal = yaffs_mknod(dir, dentry, mode | S_IFDIR, 0);
#if 0
	/* attempt to fix dir bug - didn't work */
	if (!retVal) {
		dget(dentry);
	}
#endif
	return retVal;
}

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))
static int yaffs_create(struct inode *dir, struct dentry *dentry, int mode,
			struct nameidata *n)
#else
static int yaffs_create(struct inode *dir, struct dentry *dentry, int mode)
#endif
{
	T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_create\n"));
	return yaffs_mknod(dir, dentry, mode | S_IFREG, 0);
}

static int yaffs_unlink(struct inode *dir, struct dentry *dentry)
{
	int retVal;

	yaffs_Device *dev;

	T(YAFFS_TRACE_OS,
	  (KERN_DEBUG "yaffs_unlink %d:%s\n", (int)(dir->i_ino),
	   dentry->d_name.name));

	dev = yaffs_InodeToObject(dir)->myDev;

	yaffs_GrossLock(dev);

	retVal = yaffs_Unlink(yaffs_InodeToObject(dir), dentry->d_name.name);

	if (retVal == YAFFS_OK) {
		dentry->d_inode->i_nlink--;
		dir->i_version++;
		yaffs_GrossUnlock(dev);
		mark_inode_dirty(dentry->d_inode);
		return 0;
	}
	yaffs_GrossUnlock(dev);
	return -ENOTEMPTY;
}

/*
 * Create a link...
 */
static int yaffs_link(struct dentry *old_dentry, struct inode *dir,
		      struct dentry *dentry)
{
	struct inode *inode = old_dentry->d_inode;
	yaffs_Object *obj = NULL;
	yaffs_Object *link = NULL;
	yaffs_Device *dev;

	T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_link\n"));

	obj = yaffs_InodeToObject(inode);
	dev = obj->myDev;

	yaffs_GrossLock(dev);

	if (!S_ISDIR(inode->i_mode))	/* Don't link directories */
	{
		link =
		    yaffs_Link(yaffs_InodeToObject(dir), dentry->d_name.name,
			       obj);
	}

	if (link) {
		old_dentry->d_inode->i_nlink = yaffs_GetObjectLinkCount(obj);
		d_instantiate(dentry, old_dentry->d_inode);
		atomic_inc(&old_dentry->d_inode->i_count);
		T(YAFFS_TRACE_OS,
		  (KERN_DEBUG "yaffs_link link count %d i_count %d\n",
		   old_dentry->d_inode->i_nlink,
		   atomic_read(&old_dentry->d_inode->i_count)));

	}

	yaffs_GrossUnlock(dev);

	if (link) {

		return 0;
	}

	return -EPERM;
}

static int yaffs_symlink(struct inode *dir, struct dentry *dentry,
			 const char *symname)
{
	yaffs_Object *obj;
	yaffs_Device *dev;
	uid_t uid = current->fsuid;
	gid_t gid = (dir->i_mode & S_ISGID) ? dir->i_gid : current->fsgid;

	T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_symlink\n"));

	dev = yaffs_InodeToObject(dir)->myDev;
	yaffs_GrossLock(dev);
	obj = yaffs_MknodSymLink(yaffs_InodeToObject(dir), dentry->d_name.name,
				 S_IFLNK | S_IRWXUGO, uid, gid, symname);
	yaffs_GrossUnlock(dev);

	if (obj) {

		struct inode *inode;

		inode = yaffs_get_inode(dir->i_sb, obj->yst_mode, 0, obj);
		d_instantiate(dentry, inode);
		T(YAFFS_TRACE_OS, (KERN_DEBUG "symlink created OK\n"));
		return 0;
	} else {
		T(YAFFS_TRACE_OS, (KERN_DEBUG "symlink not created\n"));

	}

	return -ENOMEM;
}

static int yaffs_sync_object(struct file *file, struct dentry *dentry,
			     int datasync)
{

	yaffs_Object *obj;
	yaffs_Device *dev;

	obj = yaffs_DentryToObject(dentry);

	dev = obj->myDev;

	T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_sync_object\n"));
	yaffs_GrossLock(dev);
	yaffs_FlushFile(obj, 1);
	yaffs_GrossUnlock(dev);
	return 0;
}

/*
 * The VFS layer already does all the dentry stuff for rename.
 *
 * NB: POSIX says you can rename an object over an old object of the same name
 */
static int yaffs_rename(struct inode *old_dir, struct dentry *old_dentry,
			struct inode *new_dir, struct dentry *new_dentry)
{
	yaffs_Device *dev;
	int retVal = YAFFS_FAIL;
	yaffs_Object *target;

        T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_rename\n"));
	dev = yaffs_InodeToObject(old_dir)->myDev;

	yaffs_GrossLock(dev);

	/* Check if the target is an existing directory that is not empty. */
	target =
	    yaffs_FindObjectByName(yaffs_InodeToObject(new_dir),
				   new_dentry->d_name.name);
	
	

	if (target &&
	    target->variantType == YAFFS_OBJECT_TYPE_DIRECTORY &&
	    !list_empty(&target->variant.directoryVariant.children)) {
	    
	        T(YAFFS_TRACE_OS, (KERN_DEBUG "target is non-empty dir\n"));

		retVal = YAFFS_FAIL;
	} else {

		/* Now does unlinking internally using shadowing mechanism */
	        T(YAFFS_TRACE_OS, (KERN_DEBUG "calling yaffs_RenameObject\n"));
		
		retVal =
		    yaffs_RenameObject(yaffs_InodeToObject(old_dir),
				       old_dentry->d_name.name,
				       yaffs_InodeToObject(new_dir),
				       new_dentry->d_name.name);

	}
	yaffs_GrossUnlock(dev);

	if (retVal == YAFFS_OK) {
		if(target) {
			new_dentry->d_inode->i_nlink--;
			mark_inode_dirty(new_dentry->d_inode);
		}

		return 0;
	} else {
		return -ENOTEMPTY;
	}

}

static int yaffs_setattr(struct dentry *dentry, struct iattr *attr)
{
	struct inode *inode = dentry->d_inode;
	int error;
	yaffs_Device *dev;

	T(YAFFS_TRACE_OS,
	  (KERN_DEBUG "yaffs_setattr of object %d\n",
	   yaffs_InodeToObject(inode)->objectId));

	if ((error = inode_change_ok(inode, attr)) == 0) {

		dev = yaffs_InodeToObject(inode)->myDev;
		yaffs_GrossLock(dev);
		if (yaffs_SetAttributes(yaffs_InodeToObject(inode), attr) ==
		    YAFFS_OK) {
			error = 0;
		} else {
			error = -EPERM;
		}
		yaffs_GrossUnlock(dev);
		if (!error)
			error = inode_setattr(inode, attr);
	}
	return error;
}

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,17))
static int yaffs_statfs(struct dentry *dentry, struct kstatfs *buf)
{
	yaffs_Device *dev = yaffs_DentryToObject(dentry)->myDev;
	struct super_block *sb = dentry->d_sb;
#elif (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))
static int yaffs_statfs(struct super_block *sb, struct kstatfs *buf)
{
	yaffs_Device *dev = yaffs_SuperToDevice(sb);
#else
static int yaffs_statfs(struct super_block *sb, struct statfs *buf)
{
	yaffs_Device *dev = yaffs_SuperToDevice(sb);
#endif

	T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_statfs\n"));

	yaffs_GrossLock(dev);

	buf->f_type = YAFFS_MAGIC;
	buf->f_bsize = sb->s_blocksize;
	buf->f_namelen = 255;
	if (sb->s_blocksize > dev->nDataBytesPerChunk) {

		buf->f_blocks =
		    (dev->endBlock - dev->startBlock +
		     1) * dev->nChunksPerBlock / (sb->s_blocksize /
						  dev->nDataBytesPerChunk);
		buf->f_bfree =
		    yaffs_GetNumberOfFreeChunks(dev) / (sb->s_blocksize /
							dev->nDataBytesPerChunk);
	} else {

		buf->f_blocks =
		    (dev->endBlock - dev->startBlock +
		     1) * dev->nChunksPerBlock * (dev->nDataBytesPerChunk /
						  sb->s_blocksize);
		buf->f_bfree =
		    yaffs_GetNumberOfFreeChunks(dev) * (dev->nDataBytesPerChunk /
							sb->s_blocksize);
	}
	buf->f_files = 0;
	buf->f_ffree = 0;
	buf->f_bavail = buf->f_bfree;

	yaffs_GrossUnlock(dev);
	return 0;
}



static int yaffs_do_sync_fs(struct super_block *sb)
{

	yaffs_Device *dev = yaffs_SuperToDevice(sb);
	T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_do_sync_fs\n"));

	if(sb->s_dirt) {
		yaffs_GrossLock(dev);

		if(dev)
			yaffs_CheckpointSave(dev);
		
		yaffs_GrossUnlock(dev);

		sb->s_dirt = 0;
	}
	return 0;
}


#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,17))
static void yaffs_write_super(struct super_block *sb)
#else
static int yaffs_write_super(struct super_block *sb)
#endif
{

	T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_write_super\n"));
#if (LINUX_VERSION_CODE < KERNEL_VERSION(2,6,18))
	return 0; /* yaffs_do_sync_fs(sb);*/
#endif
}


#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,17))
static int yaffs_sync_fs(struct super_block *sb, int wait)
#else
static int yaffs_sync_fs(struct super_block *sb)
#endif
{

	T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_sync_fs\n"));
	
	return 0; /* yaffs_do_sync_fs(sb);*/
	
}


static void yaffs_read_inode(struct inode *inode)
{
	/* NB This is called as a side effect of other functions, but
	 * we had to release the lock to prevent deadlocks, so 
	 * need to lock again.
	 */

	yaffs_Object *obj;
	yaffs_Device *dev = yaffs_SuperToDevice(inode->i_sb);

	T(YAFFS_TRACE_OS,
	  (KERN_DEBUG "yaffs_read_inode for %d\n", (int)inode->i_ino));

	yaffs_GrossLock(dev);
	
	obj = yaffs_FindObjectByNumber(dev, inode->i_ino);

	yaffs_FillInodeFromObject(inode, obj);

	yaffs_GrossUnlock(dev);
}

static LIST_HEAD(yaffs_dev_list);

static void yaffs_put_super(struct super_block *sb)
{
	yaffs_Device *dev = yaffs_SuperToDevice(sb);

	T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_put_super\n"));

	yaffs_GrossLock(dev);
	
	yaffs_FlushEntireDeviceCache(dev);
	
	if (dev->putSuperFunc) {
		dev->putSuperFunc(sb);
	}
	
	yaffs_CheckpointSave(dev);
	yaffs_Deinitialise(dev);
	
	yaffs_GrossUnlock(dev);

	/* we assume this is protected by lock_kernel() in mount/umount */
	list_del(&dev->devList);
	
	if(dev->spareBuffer){
		YFREE(dev->spareBuffer);
		dev->spareBuffer = NULL;
	}

	kfree(dev);
}


static void yaffs_MTDPutSuper(struct super_block *sb)
{

	struct mtd_info *mtd = yaffs_SuperToDevice(sb)->genericDevice;

	if (mtd->sync) {
		mtd->sync(mtd);
	}

	put_mtd_device(mtd);
}


static void yaffs_MarkSuperBlockDirty(void *vsb)
{
	struct super_block *sb = (struct super_block *)vsb;
	
	T(YAFFS_TRACE_OS, (KERN_DEBUG "yaffs_MarkSuperBlockDirty() sb = %p\n",sb));
//	if(sb)
//		sb->s_dirt = 1;
}

static struct super_block *yaffs_internal_read_super(int yaffsVersion,
						     struct super_block *sb,
						     void *data, int silent)
{
	int nBlocks;
	struct inode *inode = NULL;
	struct dentry *root;
	yaffs_Device *dev = 0;
	char devname_buf[BDEVNAME_SIZE + 1];
	struct mtd_info *mtd;
	int err;

	sb->s_magic = YAFFS_MAGIC;
	sb->s_op = &yaffs_super_ops;

	if (!sb)
		printk(KERN_INFO "yaffs: sb is NULL\n");
	else if (!sb->s_dev)
		printk(KERN_INFO "yaffs: sb->s_dev is NULL\n");
	else if (!yaffs_devname(sb, devname_buf))
		printk(KERN_INFO "yaffs: devname is NULL\n");
	else
		printk(KERN_INFO "yaffs: dev is %d name is \"%s\"\n",
		       sb->s_dev,
		       yaffs_devname(sb, devname_buf));

	sb->s_blocksize = PAGE_CACHE_SIZE;
	sb->s_blocksize_bits = PAGE_CACHE_SHIFT;
	T(YAFFS_TRACE_OS, ("yaffs_read_super: Using yaffs%d\n", yaffsVersion));
	T(YAFFS_TRACE_OS,
	  ("yaffs_read_super: block size %d\n", (int)(sb->s_blocksize)));

#ifdef CONFIG_YAFFS_DISABLE_WRITE_VERIFY
	T(YAFFS_TRACE_OS,
	  ("yaffs: Write verification disabled. All guarantees "
	   "null and void\n"));
#endif

	T(YAFFS_TRACE_ALWAYS, ("yaffs: Attempting MTD mount on %u.%u, "
			       "\"%s\"\n",
			       MAJOR(sb->s_dev), MINOR(sb->s_dev),
			       yaffs_devname(sb, devname_buf)));

	/* Check it's an mtd device..... */
	if (MAJOR(sb->s_dev) != MTD_BLOCK_MAJOR) {
		return NULL;	/* This isn't an mtd device */
	}
	/* Get the device */
	mtd = get_mtd_device(NULL, MINOR(sb->s_dev));
	if (!mtd) {
		T(YAFFS_TRACE_ALWAYS,
		  ("yaffs: MTD device #%u doesn't appear to exist\n",
		   MINOR(sb->s_dev)));
		return NULL;
	}
	/* Check it's NAND */
	if (mtd->type != MTD_NANDFLASH) {
		T(YAFFS_TRACE_ALWAYS,
		  ("yaffs: MTD device is not NAND it's type %d\n", mtd->type));
		return NULL;
	}

	T(YAFFS_TRACE_OS, (" erase %p\n", mtd->erase));
	T(YAFFS_TRACE_OS, (" read %p\n", mtd->read));
	T(YAFFS_TRACE_OS, (" write %p\n", mtd->write));
	T(YAFFS_TRACE_OS, (" readoob %p\n", mtd->read_oob));
	T(YAFFS_TRACE_OS, (" writeoob %p\n", mtd->write_oob));
	T(YAFFS_TRACE_OS, (" block_isbad %p\n", mtd->block_isbad));
	T(YAFFS_TRACE_OS, (" block_markbad %p\n", mtd->block_markbad));
#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,17))
	T(YAFFS_TRACE_OS, (" writesize %d\n", mtd->writesize));
#else
	T(YAFFS_TRACE_OS, (" oobblock %d\n", mtd->oobblock));
#endif
	T(YAFFS_TRACE_OS, (" oobsize %d\n", mtd->oobsize));
	T(YAFFS_TRACE_OS, (" erasesize %d\n", mtd->erasesize));
	T(YAFFS_TRACE_OS, (" size %d\n", mtd->size));
	
#ifdef CONFIG_YAFFS_AUTO_YAFFS2

	if (yaffsVersion == 1 && 
#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,17))
	    mtd->writesize >= 2048) {
#else
	    mtd->oobblock >= 2048) {
#endif
	    T(YAFFS_TRACE_ALWAYS,("yaffs: auto selecting yaffs2\n"));
	    yaffsVersion = 2;
	}	
	
	/* Added NCB 26/5/2006 for completeness */
	if (yaffsVersion == 2 && 
#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,17))
	    mtd->writesize == 512) {
#else
	    mtd->oobblock >= 512) {
#endif
	    T(YAFFS_TRACE_ALWAYS,("yaffs: auto selecting yaffs1\n"));
	    yaffsVersion = 1;
	}	

#endif

	if (yaffsVersion == 2) {
		/* Check for version 2 style functions */
		if (!mtd->erase ||
		    !mtd->block_isbad ||
		    !mtd->block_markbad ||
		    !mtd->read ||
		    !mtd->write ||
#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,17))
		    !mtd->read_oob || !mtd->write_oob) {
#else
		    !mtd->write_ecc ||
		    !mtd->read_ecc || !mtd->read_oob || !mtd->write_oob) {
#endif
			T(YAFFS_TRACE_ALWAYS,
			  ("yaffs: MTD device does not support required "
			   "functions\n"));;
			return NULL;
		}

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,17))
		if (mtd->writesize < YAFFS_MIN_YAFFS2_CHUNK_SIZE ||
#else
		if (mtd->oobblock < YAFFS_MIN_YAFFS2_CHUNK_SIZE ||
#endif
		    mtd->oobsize < YAFFS_MIN_YAFFS2_SPARE_SIZE) {
			T(YAFFS_TRACE_ALWAYS,
			  ("yaffs: MTD device does not have the "
			   "right page sizes\n"));
			return NULL;
		}
	} else {
		/* Check for V1 style functions */
		if (!mtd->erase ||
		    !mtd->read ||
		    !mtd->write ||
#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,17))
		    !mtd->read_oob || !mtd->write_oob) {
#else
		    !mtd->write_ecc ||
		    !mtd->read_ecc || !mtd->read_oob || !mtd->write_oob) {
#endif
			T(YAFFS_TRACE_ALWAYS,
			  ("yaffs: MTD device does not support required "
			   "functions\n"));;
			return NULL;
		}

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,17))
		if (mtd->writesize < YAFFS_BYTES_PER_CHUNK ||
#else
		if (mtd->oobblock < YAFFS_BYTES_PER_CHUNK ||
#endif
		    mtd->oobsize != YAFFS_BYTES_PER_SPARE) {
			T(YAFFS_TRACE_ALWAYS,
			  ("yaffs: MTD device does not support have the "
			   "right page sizes\n"));
			return NULL;
		}
	}

	/* OK, so if we got here, we have an MTD that's NAND and looks
	 * like it has the right capabilities
	 * Set the yaffs_Device up for mtd
	 */

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))
	sb->s_fs_info = dev = kmalloc(sizeof(yaffs_Device), GFP_KERNEL);
#else
	sb->u.generic_sbp = dev = kmalloc(sizeof(yaffs_Device), GFP_KERNEL);
#endif
	if (!dev) {
		/* Deep shit could not allocate device structure */
		T(YAFFS_TRACE_ALWAYS,
		  ("yaffs_read_super: Failed trying to allocate "
		   "yaffs_Device. \n"));
		return NULL;
	}

	memset(dev, 0, sizeof(yaffs_Device));
	dev->genericDevice = mtd;
	dev->name = mtd->name;

	/* Set up the memory size parameters.... */

	nBlocks = mtd->size / (YAFFS_CHUNKS_PER_BLOCK * YAFFS_BYTES_PER_CHUNK);
	dev->startBlock = 0;
	dev->endBlock = nBlocks - 1;
	dev->nChunksPerBlock = YAFFS_CHUNKS_PER_BLOCK;
	dev->nDataBytesPerChunk = YAFFS_BYTES_PER_CHUNK;
	dev->nReservedBlocks = 5;
	dev->nShortOpCaches = 10;	/* Enable short op caching */

	/* ... and the functions. */
	if (yaffsVersion == 2) {
		dev->writeChunkWithTagsToNAND =
		    nandmtd2_WriteChunkWithTagsToNAND;
		dev->readChunkWithTagsFromNAND =
		    nandmtd2_ReadChunkWithTagsFromNAND;
		dev->markNANDBlockBad = nandmtd2_MarkNANDBlockBad;
		dev->queryNANDBlock = nandmtd2_QueryNANDBlock;
		dev->spareBuffer = YMALLOC(mtd->oobsize);
		dev->isYaffs2 = 1;
#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,17))
		dev->nDataBytesPerChunk = mtd->writesize;
		dev->nChunksPerBlock = mtd->erasesize / mtd->writesize;
#else
		dev->nDataBytesPerChunk = mtd->oobblock;
		dev->nChunksPerBlock = mtd->erasesize / mtd->oobblock;
#endif
		nBlocks = mtd->size / mtd->erasesize;

		dev->nCheckpointReservedBlocks = 10;
		dev->startBlock = 0;
		dev->endBlock = nBlocks - 1;
	} else {
		dev->writeChunkToNAND = nandmtd_WriteChunkToNAND;
		dev->readChunkFromNAND = nandmtd_ReadChunkFromNAND;
		dev->isYaffs2 = 0;
	}
	/* ... and common functions */
	dev->eraseBlockInNAND = nandmtd_EraseBlockInNAND;
	dev->initialiseNAND = nandmtd_InitialiseNAND;

	dev->putSuperFunc = yaffs_MTDPutSuper;
	
	dev->superBlock = (void *)sb;
	dev->markSuperBlockDirty = yaffs_MarkSuperBlockDirty;
	

#ifndef CONFIG_YAFFS_DOES_ECC
	dev->useNANDECC = 1;
#endif

#ifdef CONFIG_YAFFS_DISABLE_WIDE_TNODES
	dev->wideTnodesDisabled = 1;
#endif

	/* we assume this is protected by lock_kernel() in mount/umount */
	list_add_tail(&dev->devList, &yaffs_dev_list);

	init_MUTEX(&dev->grossLock);

	yaffs_GrossLock(dev);

#ifdef CONFIG_YAFFS_ERASE_MOUNT_OPTION
	if(data && strcmp((char *) data, "erase") == 0) {
		int i;
		T(YAFFS_TRACE_ALWAYS,
		  ("yaffs_read_super: Erasing device\n"));
		for(i = 0; i < nBlocks; i++)
			dev->eraseBlockInNAND(dev, i);
	}
#endif

	err = yaffs_GutsInitialise(dev);

	T(YAFFS_TRACE_OS,
	  ("yaffs_read_super: guts initialised %s\n",
	   (err == YAFFS_OK) ? "OK" : "FAILED"));
	
	/* Release lock before yaffs_get_inode() */
	yaffs_GrossUnlock(dev);

	/* Create root inode */
	if (err == YAFFS_OK)
		inode = yaffs_get_inode(sb, S_IFDIR | 0755, 0,
					yaffs_Root(dev));

	if (!inode)
		return NULL;

	inode->i_op = &yaffs_dir_inode_operations;
	inode->i_fop = &yaffs_dir_operations;

	T(YAFFS_TRACE_OS, ("yaffs_read_super: got root inode\n"));

	root = d_alloc_root(inode);

	T(YAFFS_TRACE_OS, ("yaffs_read_super: d_alloc_root done\n"));

	if (!root) {
		iput(inode);
		return NULL;
	}
	sb->s_root = root;

	T(YAFFS_TRACE_OS, ("yaffs_read_super: done\n"));
	return sb;
}


#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))
static int yaffs_internal_read_super_mtd(struct super_block *sb, void *data,
					 int silent)
{
	return yaffs_internal_read_super(1, sb, data, silent) ? 0 : -EINVAL;
}

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,17))
static int yaffs_read_super(struct file_system_type *fs,
			    int flags, const char *dev_name,
			    void *data, struct vfsmount *mnt)
{

	return get_sb_bdev(fs, flags, dev_name, data,
			   yaffs_internal_read_super_mtd, mnt);
}
#else
static struct super_block *yaffs_read_super(struct file_system_type *fs,
					    int flags, const char *dev_name,
					    void *data)
{

	return get_sb_bdev(fs, flags, dev_name, data,
			   yaffs_internal_read_super_mtd);
}
#endif

static struct file_system_type yaffs_fs_type = {
	.owner = THIS_MODULE,
	.name = "yaffs",
	.get_sb = yaffs_read_super,
	.kill_sb = kill_block_super,
	.fs_flags = FS_REQUIRES_DEV,
};
#else
static struct super_block *yaffs_read_super(struct super_block *sb, void *data,
					    int silent)
{
	return yaffs_internal_read_super(1, sb, data, silent);
}

static DECLARE_FSTYPE(yaffs_fs_type, "yaffs", yaffs_read_super,
		      FS_REQUIRES_DEV);
#endif


#ifdef CONFIG_YAFFS_YAFFS2

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,5,0))
static int yaffs2_internal_read_super_mtd(struct super_block *sb, void *data,
					  int silent)
{
	return yaffs_internal_read_super(2, sb, data, silent) ? 0 : -EINVAL;
}

#if (LINUX_VERSION_CODE > KERNEL_VERSION(2,6,17))
static int yaffs2_read_super(struct file_system_type *fs,
			int flags, const char *dev_name, void *data,
			struct vfsmount *mnt)
{
	return get_sb_bdev(fs, flags, dev_name, data,
			yaffs2_internal_read_super_mtd, mnt);
}
#else
static struct super_block *yaffs2_read_super(struct file_system_type *fs,
					     int flags, const char *dev_name,
					     void *data)
{

	return get_sb_bdev(fs, flags, dev_name, data,
			   yaffs2_internal_read_super_mtd);
}
#endif

static struct file_system_type yaffs2_fs_type = {
	.owner = THIS_MODULE,
	.name = "yaffs2",
	.get_sb = yaffs2_read_super,
	.kill_sb = kill_block_super,
	.fs_flags = FS_REQUIRES_DEV,
};
#else
static struct super_block *yaffs2_read_super(struct super_block *sb,
					     void *data, int silent)
{
	return yaffs_internal_read_super(2, sb, data, silent);
}

static DECLARE_FSTYPE(yaffs2_fs_type, "yaffs2", yaffs2_read_super,
		      FS_REQUIRES_DEV);
#endif

#endif				/* CONFIG_YAFFS_YAFFS2 */

static struct proc_dir_entry *my_proc_entry;

static char *yaffs_dump_dev(char *buf, yaffs_Device * dev)
{
	buf += sprintf(buf, "startBlock......... %d\n", dev->startBlock);
	buf += sprintf(buf, "endBlock........... %d\n", dev->endBlock);
	buf += sprintf(buf, "chunkGroupBits..... %d\n", dev->chunkGroupBits);
	buf += sprintf(buf, "chunkGroupSize..... %d\n", dev->chunkGroupSize);
	buf += sprintf(buf, "nErasedBlocks...... %d\n", dev->nErasedBlocks);
	buf += sprintf(buf, "nTnodesCreated..... %d\n", dev->nTnodesCreated);
	buf += sprintf(buf, "nFreeTnodes........ %d\n", dev->nFreeTnodes);
	buf += sprintf(buf, "nObjectsCreated.... %d\n", dev->nObjectsCreated);
	buf += sprintf(buf, "nFreeObjects....... %d\n", dev->nFreeObjects);
	buf += sprintf(buf, "nFreeChunks........ %d\n", dev->nFreeChunks);
	buf += sprintf(buf, "nPageWrites........ %d\n", dev->nPageWrites);
	buf += sprintf(buf, "nPageReads......... %d\n", dev->nPageReads);
	buf += sprintf(buf, "nBlockErasures..... %d\n", dev->nBlockErasures);
	buf += sprintf(buf, "nGCCopies.......... %d\n", dev->nGCCopies);
	buf +=
	    sprintf(buf, "garbageCollections. %d\n", dev->garbageCollections);
	buf +=
	    sprintf(buf, "passiveGCs......... %d\n",
		    dev->passiveGarbageCollections);
	buf += sprintf(buf, "nRetriedWrites..... %d\n", dev->nRetriedWrites);
	buf += sprintf(buf, "nRetireBlocks...... %d\n", dev->nRetiredBlocks);
	buf += sprintf(buf, "eccFixed........... %d\n", dev->eccFixed);
	buf += sprintf(buf, "eccUnfixed......... %d\n", dev->eccUnfixed);
	buf += sprintf(buf, "tagsEccFixed....... %d\n", dev->tagsEccFixed);
	buf += sprintf(buf, "tagsEccUnfixed..... %d\n", dev->tagsEccUnfixed);
	buf += sprintf(buf, "cacheHits.......... %d\n", dev->cacheHits);
	buf += sprintf(buf, "nDeletedFiles...... %d\n", dev->nDeletedFiles);
	buf += sprintf(buf, "nUnlinkedFiles..... %d\n", dev->nUnlinkedFiles);
	buf +=
	    sprintf(buf, "nBackgroudDeletions %d\n", dev->nBackgroundDeletions);
	buf += sprintf(buf, "useNANDECC......... %d\n", dev->useNANDECC);
	buf += sprintf(buf, "isYaffs2........... %d\n", dev->isYaffs2);

	return buf;
}

static int yaffs_proc_read(char *page,
			   char **start,
			   off_t offset, int count, int *eof, void *data)
{
	struct list_head *item;
	char *buf = page;
	int step = offset;
	int n = 0;

	/* Get proc_file_read() to step 'offset' by one on each sucessive call.
	 * We use 'offset' (*ppos) to indicate where we are in devList.
	 * This also assumes the user has posted a read buffer large
	 * enough to hold the complete output; but that's life in /proc.
	 */

	*(int *)start = 1;

	/* Print header first */
	if (step == 0) {
		buf += sprintf(buf, "YAFFS built:" __DATE__ " " __TIME__
			       "\n%s\n%s\n", yaffs_fs_c_version,
			       yaffs_guts_c_version);
	}

	/* hold lock_kernel while traversing yaffs_dev_list */
	lock_kernel();

	/* Locate and print the Nth entry.  Order N-squared but N is small. */
	list_for_each(item, &yaffs_dev_list) {
		yaffs_Device *dev = list_entry(item, yaffs_Device, devList);
		if (n < step) {
			n++;
			continue;
		}
		buf += sprintf(buf, "\nDevice %d \"%s\"\n", n, dev->name);
		buf = yaffs_dump_dev(buf, dev);
		break;
	}
	unlock_kernel();

	return buf - page < count ? buf - page : count;
}

/**
 * Set the verbosity of the warnings and error messages.
 *
 */

static struct {
	char *mask_name;
	unsigned mask_bitfield;
} mask_flags[] = {
	{"allocate", YAFFS_TRACE_ALLOCATE},
	{"always", YAFFS_TRACE_ALWAYS},
	{"bad_blocks", YAFFS_TRACE_BAD_BLOCKS},
	{"buffers", YAFFS_TRACE_BUFFERS},
	{"bug", YAFFS_TRACE_BUG},
	{"deletion", YAFFS_TRACE_DELETION},
	{"erase", YAFFS_TRACE_ERASE},
	{"error", YAFFS_TRACE_ERROR},
	{"gc_detail", YAFFS_TRACE_GC_DETAIL},
	{"gc", YAFFS_TRACE_GC},
	{"mtd", YAFFS_TRACE_MTD},
	{"nandaccess", YAFFS_TRACE_NANDACCESS},
	{"os", YAFFS_TRACE_OS},
	{"scan_debug", YAFFS_TRACE_SCAN_DEBUG},
	{"scan", YAFFS_TRACE_SCAN},
	{"tracing", YAFFS_TRACE_TRACING},
	{"write", YAFFS_TRACE_WRITE},
	{"all", 0xffffffff},
	{"none", 0},
	{NULL, 0},
};

static int yaffs_proc_write(struct file *file, const char *buf,
					 unsigned long count, void *data)
{
	unsigned rg = 0, mask_bitfield;
	char *end, *mask_name;
	int i;
	int done = 0;
	int add, len;
	int pos = 0;

	rg = yaffs_traceMask;

	while (!done && (pos < count)) {
		done = 1;
		while ((pos < count) && isspace(buf[pos])) {
			pos++;
		}

		switch (buf[pos]) {
		case '+':
		case '-':
		case '=':
			add = buf[pos];
			pos++;
			break;

		default:
			add = ' ';
			break;
		}
		mask_name = NULL;
		mask_bitfield = simple_strtoul(buf + pos, &end, 0);
		if (end > buf + pos) {
			mask_name = "numeral";
			len = end - (buf + pos);
			done = 0;
		} else {

			for (i = 0; mask_flags[i].mask_name != NULL; i++) {
				len = strlen(mask_flags[i].mask_name);
				if (strncmp(buf + pos, mask_flags[i].mask_name, len) == 0) {
					mask_name = mask_flags[i].mask_name;
					mask_bitfield = mask_flags[i].mask_bitfield;
					done = 0;
					break;
				}
			}
		}

		if (mask_name != NULL) {
			pos += len;
			done = 0;
			switch(add) {
			case '-':
				rg &= ~mask_bitfield;
				break;
			case '+':
				rg |= mask_bitfield;
				break;
			case '=':
				rg = mask_bitfield;
				break;
			default:
				rg |= mask_bitfield;
				break;
			}
		}
	}

	yaffs_traceMask = rg;
	if (rg & YAFFS_TRACE_ALWAYS) {
		for (i = 0; mask_flags[i].mask_name != NULL; i++) {
			char flag;
			flag = ((rg & mask_flags[i].mask_bitfield) == mask_flags[i].mask_bitfield) ? '+' : '-';
			printk("%c%s\n", flag, mask_flags[i].mask_name);
		}
	}

	return count;
}

/* Stuff to handle installation of file systems */
struct file_system_to_install {
	struct file_system_type *fst;
	int installed;
};

static struct file_system_to_install fs_to_install[] = {
//#ifdef CONFIG_YAFFS_YAFFS1
	{&yaffs_fs_type, 0},
//#endif
//#ifdef CONFIG_YAFFS_YAFFS2
	{&yaffs2_fs_type, 0},
//#endif
	{NULL, 0}
};

static int __init init_yaffs_fs(void)
{
	int error = 0;
	struct file_system_to_install *fsinst;

	T(YAFFS_TRACE_ALWAYS,
	  ("yaffs " __DATE__ " " __TIME__ " Installing. \n"));

	/* Install the proc_fs entry */
	my_proc_entry = create_proc_entry("yaffs",
					       S_IRUGO | S_IFREG,
					       &proc_root);

	if (my_proc_entry) {
		my_proc_entry->write_proc = yaffs_proc_write;
		my_proc_entry->read_proc = yaffs_proc_read;
		my_proc_entry->data = NULL;
	} else {
		return -ENOMEM;
	}

	/* Now add the file system entries */

	fsinst = fs_to_install;

	while (fsinst->fst && !error) {
		error = register_filesystem(fsinst->fst);
		if (!error) {
			fsinst->installed = 1;
		}
		fsinst++;
	}

	/* Any errors? uninstall  */
	if (error) {
		fsinst = fs_to_install;

		while (fsinst->fst) {
			if (fsinst->installed) {
				unregister_filesystem(fsinst->fst);
				fsinst->installed = 0;
			}
			fsinst++;
		}
	}

	return error;
}

static void __exit exit_yaffs_fs(void)
{

	struct file_system_to_install *fsinst;

	T(YAFFS_TRACE_ALWAYS, ("yaffs " __DATE__ " " __TIME__
			       " removing. \n"));

	remove_proc_entry("yaffs", &proc_root);

	fsinst = fs_to_install;

	while (fsinst->fst) {
		if (fsinst->installed) {
			unregister_filesystem(fsinst->fst);
			fsinst->installed = 0;
		}
		fsinst++;
	}

}

module_init(init_yaffs_fs)
module_exit(exit_yaffs_fs)

MODULE_DESCRIPTION("YAFFS2 - a NAND specific flash file system");
MODULE_AUTHOR("Charles Manning, Aleph One Ltd., 2002-2006");
MODULE_LICENSE("GPL");
