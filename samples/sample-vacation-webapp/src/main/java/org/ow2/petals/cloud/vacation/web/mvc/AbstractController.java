/**
 * Copyright (c) 2015-2016 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.cloud.vacation.web.mvc;

import org.ow2.petals.cloud.vacation.web.UserSession;
import org.ow2.petals.cloud.vacation.web.UserSession.NeedUserException;
import org.ow2.petals.cloud.vacation.web.services.VacationClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.servlet.ModelAndView;

/**
 * 
 * @author vnoel
 *
 */
public abstract class AbstractController {

    @Autowired
    protected UserSession userSession;

    @Autowired
    protected VacationClient vacationClient;

    @ExceptionHandler(NeedUserException.class)
    public ModelAndView handleNeedUserException(final NeedUserException ex) {
        return new ModelAndView("redirect:/login");
    }
}
