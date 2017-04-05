/**
 * Copyright (c) 2015-2017 Linagora
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
package org.ow2.petals.flowable;

import org.activiti.bpmn.model.BaseElement;
import org.activiti.bpmn.model.ServiceTask;
import org.activiti.engine.impl.bpmn.parser.BpmnParse;
import org.activiti.engine.impl.bpmn.parser.handler.AbstractBpmnParseHandler;
import org.activiti.engine.impl.pvm.process.ActivityImpl;
import org.activiti.engine.parse.BpmnParseHandler;

/**
 * <p>
 * A {@link BpmnParseHandler} to force service task to be asynchronous.
 * </p>
 * <p>
 * When a service task is just placed after an start event, we are not able to retrieve MONIT variables when the service
 * task start. To be able to retrieve these variables, the service task MUST be asynchronous.
 * </p>
 * 
 * @see PETALSSEACTIVITI-4
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class ServiceTaskForceAsyncParseHandler extends AbstractBpmnParseHandler<ServiceTask> {

    @Override
    protected Class<? extends BaseElement> getHandledType() {
        return ServiceTask.class;
    }

    @Override
    protected void executeParse(final BpmnParse bpmnParse, final ServiceTask element) {

        // Make always async
        final ActivityImpl activity = findActivity(bpmnParse, element.getId());
        activity.setAsync(true);
    }

}
