/**
 * Copyright (c) 2014-2017 Linagora
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
package org.ow2.petals.activitibpmn.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.activiti.bpmn.converter.BpmnXMLConverter;
import org.activiti.bpmn.converter.util.InputStreamProvider;
import org.activiti.bpmn.model.BpmnModel;
import org.activiti.engine.impl.util.io.InputStreamSource;
import org.ow2.petals.activitibpmn.ActivitiSEConstants;
import org.ow2.petals.activitibpmn.exception.IncoherentProcessDefinitionDeclarationException;
import org.ow2.petals.activitibpmn.exception.InvalidVersionDeclaredException;
import org.ow2.petals.activitibpmn.exception.NoProcessDefinitionDeclarationException;
import org.ow2.petals.activitibpmn.exception.ProcessDefinitionDeclarationException;
import org.ow2.petals.activitibpmn.exception.UnexistingProcessFileException;
import org.ow2.petals.activitibpmn.incoming.operation.EmbeddedProcessDefinition;
import org.ow2.petals.component.framework.api.configuration.SuConfigurationParameters;

/**
 * BPMN file reader
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class BpmnReader {

    /**
     * The configuration extensions of the service unit from which BPMN files will be read.
     */
    private final SuConfigurationParameters extensions;

    /**
     * The root directory of the service unit from which BPMN files will be read.
     */
    private final String suRootPath;

    /**
     * The component's Logger
     */
    private final Logger logger;

    /**
     * @param extensions
     *            The configuration extensions of the service unit. Not <code>null</code>.
     * @param suRootPath
     *            The root directory of the service unit. Not <code>null</code>.
     * @param logger
     *            A {@link Logger} instance. Not <code>null</code>.
     */
    public BpmnReader(final SuConfigurationParameters extensions, final String suRootPath, final Logger logger) {
        assert extensions != null;
        assert suRootPath != null;
        assert logger != null;

        this.extensions = extensions;
        this.suRootPath = suRootPath;
        this.logger = logger;
    }

    /**
     * <p>
     * Read embedded BPMN models of the SU from their raw information
     * </p>
     * 
     * @return The map of embedded process definition containing. The map key is the process file name.
     * @param suRootPath
     *            The root directory of the service unit. Not <code>null</code>.
     * @param logger
     *            A {@link Logger} instance. Not <code>null</code>.
     * @throws ProcessDefinitionDeclarationException
     *             A raw data of the SU JBI descriptor is invalid
     */
    public Map<String, EmbeddedProcessDefinition> readBpmnModels() throws ProcessDefinitionDeclarationException {

        final Map<String, EmbeddedProcessDefinition> bpmnModels = new HashMap<String, EmbeddedProcessDefinition>();

        String processFileName = this.extensions.get(ActivitiSEConstants.PROCESS_FILE);
        String versionStr = this.extensions.get(ActivitiSEConstants.VERSION);
        if (processFileName == null && versionStr == null) {
            // The SU does not contain an unique process definition, perhaps several process definitions

            // TODO: Multi-process definitions should be reviewed because a wrapper tag is missing: The right writing
            // style would be:
            // <process-definition>
            // <process_file>...</process_file>
            // <version>...</version>
            // </process-definition>
            // But the CDK provides a mechanism using increment.
            int nbProcesses = 1;
            do {
                processFileName = this.extensions.get(ActivitiSEConstants.PROCESS_FILE + nbProcesses);
                versionStr = this.extensions.get(ActivitiSEConstants.VERSION + nbProcesses);
                if (nbProcesses == 1 && processFileName == null && versionStr == null) {
                    throw new NoProcessDefinitionDeclarationException();
                } else if (processFileName != null && versionStr != null) {
                    bpmnModels.put(processFileName, this.readBpmnModel(processFileName, versionStr));
                    nbProcesses++;
                } else if ((processFileName != null && versionStr == null)
                        || (processFileName == null && versionStr != null)) {
                    throw new IncoherentProcessDefinitionDeclarationException(processFileName, versionStr);
                } else {
                    // Here, processFileName == null and versionStr == null, and at least one process definition was
                    // previously read, so we have nothing to do, we will exit the loop
                }
            } while (processFileName != null && versionStr != null);
        } else if (processFileName != null && versionStr != null) {
            // One process description
            bpmnModels.put(processFileName, this.readBpmnModel(processFileName, versionStr));
        } else {
            throw new IncoherentProcessDefinitionDeclarationException(processFileName, versionStr);
        }

        return bpmnModels;
    }

    /**
     * <p>
     * Read an embedded BPMN model of the SU from its raw information.
     * </p>
     * 
     * @param processFileName
     *            The file name of the process, as raw data read from the SU JBI descriptor. Not <code>null</code>.
     * @param versionStr
     *            The version of the process, as raw data read from the SU JBI descriptor. Not <code>null</code>.
     * @throws ProcessDefinitionDeclarationException
     *             A raw data of the SU JBI descriptor is invalid
     */
    private EmbeddedProcessDefinition readBpmnModel(final String processFileName, final String versionStr)
            throws ProcessDefinitionDeclarationException {

        if (processFileName != null && processFileName.trim().isEmpty()) {
            throw new IncoherentProcessDefinitionDeclarationException(processFileName, versionStr);
        }

        final int version;
        if (versionStr != null && versionStr.trim().isEmpty()) {
            throw new IncoherentProcessDefinitionDeclarationException(processFileName, versionStr);
        } else {
            try {
                version = Integer.parseInt(versionStr);
            } catch (final NumberFormatException e) {
                throw new InvalidVersionDeclaredException(processFileName, versionStr);
            }
        }

        // Read the BPMN model
        final File processFile = new File(this.suRootPath, processFileName);
        try {
            final FileInputStream bpmnInputFile = new FileInputStream(processFile);
            try {
                final InputStreamProvider bpmnInputStreamSource = new InputStreamSource(bpmnInputFile);

                // add BPMN Model in order to change the category in the Process definition
                // that is indeed derived from the targetNameSpace of the bpmn20.xml file
                // TODO: Enable validation
                final BpmnModel bpmnModel = new BpmnXMLConverter().convertToBpmnModel(bpmnInputStreamSource, false,
                        false);

                // TODO manage the assignee according with su jbi descriptor

                if (this.logger.isLoggable(Level.FINE)) {
                    this.logger.fine("The BPMN process [file: " + processFileName + ", version: " + version
                            + "] is succesfully read");
                }

                return new EmbeddedProcessDefinition(processFileName, version, bpmnModel);

            } finally {
                try {
                    bpmnInputFile.close();
                } catch (final IOException e) {
                    this.logger.log(Level.WARNING, "Unable to close BPMN definition file ''. Error skiped !", e);
                }
            }

        } catch (final FileNotFoundException e) {
            throw new UnexistingProcessFileException(processFile.getAbsolutePath(), e);
        }
    }

}
