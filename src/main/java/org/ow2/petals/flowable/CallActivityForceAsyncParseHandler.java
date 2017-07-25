/**
 * Copyright (c) 2017 Linagora
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
import org.flowable.bpmn.model.CallActivity;
import org.flowable.bpmn.model.FlowElement;
import org.flowable.bpmn.model.FlowNode;
import org.flowable.engine.impl.bpmn.parser.BpmnParse;
import org.flowable.engine.impl.bpmn.parser.handler.AbstractBpmnParseHandler;
import org.flowable.engine.parse.BpmnParseHandler;

/**
 * <p>
 * A {@link BpmnParseHandler} to force call activity to be asynchronous.
 * </p>
 * <p>
 * When a call activity is just placed after an start event, we are not able to retrieve MONIT variables when the call
 * activity starts. To be able to retrieve these variables, the call activity task MUST be asynchronous.
 * </p>
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class CallActivityForceAsyncParseHandler extends AbstractBpmnParseHandler<CallActivity> {

    private final Logger log;

    public CallActivityForceAsyncParseHandler(final Logger log) {
        this.log = log;
    }

    @Override
    protected Class<? extends BaseElement> getHandledType() {
        return CallActivity.class;
    }

    @Override
    protected void executeParse(final BpmnParse bpmnParse, final CallActivity element) {

        // Make always async
        final FlowElement callActivityFlowElt = bpmnParse.getCurrentProcess().getFlowElement(element.getId());
        if (callActivityFlowElt instanceof FlowNode) {
            ((FlowNode) callActivityFlowElt).setAsynchronous(true);
        } else {
            this.log.warning(String.format("Unable to find the call activity '%s' to force its asynchronous execution.",
                    element.getId()));
        }
    }

}
