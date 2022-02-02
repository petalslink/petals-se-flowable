/**
 * Copyright (c) 2015-2022 Linagora
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

import java.util.logging.Logger;

import org.flowable.bpmn.model.BaseElement;
import org.flowable.bpmn.model.FlowElement;
import org.flowable.bpmn.model.FlowNode;
import org.flowable.bpmn.model.ServiceTask;
import org.flowable.engine.impl.bpmn.parser.BpmnParse;
import org.flowable.engine.impl.bpmn.parser.handler.AbstractBpmnParseHandler;
import org.flowable.engine.parse.BpmnParseHandler;

/**
 * <p>
 * A {@link BpmnParseHandler} to force service task to be asynchronous.
 * </p>
 * <p>
 * When a service task is just placed after an start event, we are not able to retrieve MONIT variables when the service
 * task starts. To be able to retrieve these variables, the service task MUST be asynchronous.
 * </p>
 * 
 * @see <a href="https://jira.petalslink.com/browse/PETALSSEACTIVITI-4">PETALSSEACTIVITI-4</a>
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class ServiceTaskForceAsyncParseHandler extends AbstractBpmnParseHandler<ServiceTask> {

    private final Logger log;

    public ServiceTaskForceAsyncParseHandler(final Logger log) {
        this.log = log;
    }

    @Override
    protected Class<? extends BaseElement> getHandledType() {
        return ServiceTask.class;
    }

    @Override
    protected void executeParse(final BpmnParse bpmnParse, final ServiceTask element) {

        // Make always async
        final FlowElement serviceTaskFlowElt = bpmnParse.getCurrentFlowElement();
        if (serviceTaskFlowElt instanceof FlowNode) {
            ((FlowNode) serviceTaskFlowElt).setAsynchronous(true);
        } else {
            this.log.warning(String.format("Unable to find the service task '%s' to force its asynchronous execution.",
                    element.getId()));
        }
    }

}
