#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <time.h>

#include "options.h"
#include "display.h"
#include "image.h"
#include "utils.h"

static void scrot_delay(void)
{
	if (opt->delay < 1)
		return;
	if (!opt->countdown) {
		sleep(opt->delay);
		return;
	}
	printf("Taking shot in ");
	for (int i = opt->delay; i > 0; i--) {
		printf("%d.. ", i);
		fflush(stdout);
		sleep(1);
	}
	printf("0.\n");
}

static Image scrot_shoot_all_screens(void)
{
	int screens = display_num_screens();
	if (screens < 2)
		return image_from_screen();

	Image images[screens];
	for (int i = 0; i < screens; i++) {
		display_open_screen(i);
		images[i] = image_from_screen();
	}

	return image_concat(images, screens);
}

static Image scrot_shoot(void)
{
	if (opt->select || opt->window) {
		struct Area *area;
		if (opt->select)
			area = display_select_area();
		else
			area = display_select_window();
		if (area == NULL)
			util_error("Couldn't select area\n");
		scrot_delay();
		return image_from_area(area);
	}
	scrot_delay();
	if (opt->multidisp)
		return scrot_shoot_all_screens();
	return image_from_screen();
}

static Image scrot_create_thumbnail(Image image)
{
	int cwidth = image_width(image);
	int cheight = image_height(image);
	int twidth;
	int theight;

	if (opt->thumb_width > 0 || opt->thumb_height > 0) {
		if (opt->thumb_width == 0) {
			twidth = cwidth * opt->thumb_height / cheight;
			theight = opt->thumb_height;
		} else if (opt->thumb_height == 0) {
			twidth = opt->thumb_width;
			theight = cheight * opt->thumb_width / cwidth;
		} else {
			twidth = opt->thumb_width;
			theight = opt->thumb_height;
		}
	} else {
		twidth = cwidth * opt->thumb / 100;
		theight = cheight * opt->thumb / 100;
	}

	return image_scale(image, cwidth, cheight, twidth, theight);
}

int main(int argc, char **argv)
{
	options_init(argc, argv);
	if(!display_init())
		util_error("Can't open display.\n");
	image_init();

	Image image = scrot_shoot();
	if (image == NULL)
		util_error("no image grabbed\n");

    if (opt->blur)
    {
        image = image_blur(image, opt->blur);
    }

	if (opt->icon)
	{
		image = image_add_icon(image, opt->icon);
		if (!image)
			util_error("Opening %s failed\n", opt->icon);
	}

	image_set_quality(image, opt->quality);

	time_t t;
	time(&t);
	struct tm *tm = localtime(&t);

	char *path_image = util_fmt_str(opt->output_file, tm, NULL, NULL, image);
	char *path_thumb = NULL;

	bool error = image_save(image, path_image);
	if (error)
		util_error("Saving to file %s failed\n", path_image);

	if (opt->thumb != 0) {
		Image thumbnail = scrot_create_thumbnail(image);
		if (thumbnail == NULL)
			util_error("Unable to create scaled Image\n");

		path_thumb = util_fmt_str(opt->thumb_file, tm, NULL, NULL, thumbnail);
		error = image_save(thumbnail, path_thumb);
		if (error)
			util_error("Saving thumbnail %s failed\n", path_thumb);
	}

	if (opt->exec != NULL) {
		char *execstr = util_fmt_str(opt->exec, tm, path_image, path_thumb, image);
		system(execstr);
	}
}
