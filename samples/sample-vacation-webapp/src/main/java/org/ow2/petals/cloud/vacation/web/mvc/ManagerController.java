/**
 * Copyright (c) 2015-2025 Linagora
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

import org.ow2.petals.cloud.vacation.web.VacationRequest.PendingVacationRequest;
import org.ow2.petals.cloud.vacation.web.services.FlowableTaskClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

/**
 * 
 * @author vnoel
 *
 */
@Controller
@RequestMapping("/manager")
public class ManagerController extends AbstractController {

    @Autowired
    private FlowableTaskClient flowableTaskClient;

    @RequestMapping(method = RequestMethod.GET)
    public ModelAndView list() {
        final Iterable<PendingVacationRequest> requests = flowableTaskClient
                .getNewRequests(userSession.getUsernameWithChecks());
        return new ModelAndView("pages/manager", "requests", requests);
    }

    @RequestMapping(method = RequestMethod.POST, params = "accept")
    public ModelAndView accept(final @RequestParam("accept") String id, final RedirectAttributes redirect) {

        this.vacationClient.validate(id, userSession.getUsernameWithChecks(), true, "");

        redirect.addFlashAttribute("globalMessage", "Successfully accepted request " + id);

        return new ModelAndView("redirect:/manager");
    }

    @RequestMapping(method = RequestMethod.POST, params = "reject")
    public ModelAndView reject(final @RequestParam("reject") String id, final String rejectionReason,
            final RedirectAttributes redirect) {

        this.vacationClient.validate(id, userSession.getUsernameWithChecks(), false, rejectionReason);

        redirect.addFlashAttribute("globalMessage", "Successfully rejected request " + id);

        return new ModelAndView("redirect:/manager");
    }
}
