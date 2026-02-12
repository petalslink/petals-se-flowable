/**
 * Copyright (c) 2014-2026 Linagora
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
package org.ow2.petals.flowable.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.flowable.bpmn.converter.BpmnXMLConverter;
import org.flowable.bpmn.model.BpmnModel;
import org.flowable.common.engine.api.io.InputStreamProvider;
import org.flowable.common.engine.impl.util.io.InputStreamSource;
import org.flowable.engine.impl.bpmn.deployer.ResourceNameUtil;
import org.ow2.petals.component.framework.jbidescriptor.generated.Services;
import org.ow2.petals.flowable.FlowableSEConstants;
import org.ow2.petals.flowable.exception.IncoherentProcessDefinitionDeclarationException;
import org.ow2.petals.flowable.exception.InvalidSuffixForProcessFileException;
import org.ow2.petals.flowable.exception.InvalidVersionDeclaredException;
import org.ow2.petals.flowable.exception.NoProcessDefinitionDeclarationException;
import org.ow2.petals.flowable.exception.ProcessDefinitionDeclarationException;
import org.ow2.petals.flowable.exception.UnexistingProcessFileException;
import org.ow2.petals.flowable.incoming.operation.EmbeddedProcessDefinition;
import org.w3c.dom.Element;

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
    private final Services jbiServices;

    /**
     * The root directory of the service unit from which BPMN files will be read.
     */
    private final String suRootPath;

    /**
     * The component's Logger
     */
    @SuppressWarnings("squid:S1312")
    private final Logger logger;

    /**
     * @param jbiServices
     *            The configuration extensions of the service unit. Not <code>null</code>.
     * @param suRootPath
     *            The root directory of the service unit. Not <code>null</code>.
     * @param logger
     *            A {@link Logger} instance. Not <code>null</code>.
     */
    public BpmnReader(final Services jbiServices, final String suRootPath, final Logger logger) {
        assert jbiServices != null;
        assert suRootPath != null;
        assert logger != null;

        this.jbiServices = jbiServices;
        this.suRootPath = suRootPath;
        this.logger = logger;
    }

    /**
     * <p>
     * Read embedded BPMN models of the SU from their raw information
     * </p>
     * 
     * @return The map of embedded process definition containing. The map key is the process file name.
     * @throws ProcessDefinitionDeclarationException
     *             A raw data of the SU JBI descriptor is invalid
     */
    public Map<String, EmbeddedProcessDefinition> readBpmnModels() throws ProcessDefinitionDeclarationException {

        final Map<String, EmbeddedProcessDefinition> bpmnModels = new HashMap<>();

        final String uniqueProcessFileName = extractProcessFile(this.jbiServices, null);
        final String uniqueVersionStr = extractVersion(this.jbiServices, null);
        if (uniqueProcessFileName == null && uniqueVersionStr == null) {
            // The SU does not contain an unique process definition, perhaps several process definitions

            // TODO: Multi-process definitions should be reviewed because a wrapper tag is missing: The right writing
            // style would be:
            // <process-definition>
            // <process_file>...</process_file>
            // <version>...</version>
            // </process-definition>
            // But the CDK provides a mechanism using increment.
            String multiProcessFileName;
            String multiVersionStr;
            int nbProcesses = 1;
            do {
                multiProcessFileName = extractProcessFile(this.jbiServices, nbProcesses);
                multiVersionStr = extractVersion(this.jbiServices, nbProcesses);
                if (nbProcesses == 1 && multiProcessFileName == null && multiVersionStr == null) {
                    throw new NoProcessDefinitionDeclarationException();
                } else if (multiProcessFileName != null && multiVersionStr != null) {
                    bpmnModels.put(multiProcessFileName, this.readBpmnModel(multiProcessFileName, multiVersionStr));
                    nbProcesses++;
                } else if (multiProcessFileName != null || multiVersionStr != null) {
                    // Here, multiProcessFileName == null or multiVersionStr == null.
                    throw new IncoherentProcessDefinitionDeclarationException(multiProcessFileName, multiVersionStr);
                } else {
                    // Here, multiProcessFileName == null and multiVersionStr == null, and at least one process
                    // definition was previously read, so we have nothing to do, we will exit the loop.
                }
            } while (multiProcessFileName != null && multiVersionStr != null);
        } else if (uniqueProcessFileName != null && uniqueVersionStr != null) {
            // One process description
            bpmnModels.put(uniqueProcessFileName, this.readBpmnModel(uniqueProcessFileName, uniqueVersionStr));
        } else {
            throw new IncoherentProcessDefinitionDeclarationException(uniqueProcessFileName, uniqueVersionStr);
        }

        return bpmnModels;
    }

    /**
     * Extracts the BPMN process file from the JBI descriptor definition
     * 
     * @param services
     *            Extra parameters of the section 'services'
     * @param idx
     *            Counter in case of multiple process files, or {@code null}
     * @return the process file or {@code null} if not found.
     */
    private static String extractProcessFile(final Services services, final Integer idx) {
        assert services != null;

        final String tagLocalName = (idx == null ? FlowableSEConstants.PROCESS_FILE
                : FlowableSEConstants.PROCESS_FILE + idx.toString());

        final List<Element> extensions = services.getAnyOrAny();
        for (final Element e : extensions) {
            assert e != null;

            if (tagLocalName.equals(e.getLocalName())) {
                return e.getTextContent();
            }
        }

        // Here no process file was found
        return null;
    }

    /**
     * Extracts the BPMN process version from the JBI descriptor definition
     * 
     * @param services
     *            Extra parameters of the section 'services'
     * @param idx
     *            Counter in case of multiple versions, or {@code null}
     * @return the version or {@code null} if not found.
     */
    private static String extractVersion(final Services services, final Integer idx) {
        assert services != null;

        final String tagLocalName = (idx == null ? FlowableSEConstants.VERSION
                : FlowableSEConstants.VERSION + idx.toString());

        final List<Element> extensions = services.getAnyOrAny();
        for (final Element e : extensions) {
            assert e != null;

            if (tagLocalName.equals(e.getLocalName())) {
                return e.getTextContent();
            }
        }

        // Here no version was found
        return null;
    }

    /**
     * <p>
     * Read an embedded BPMN model file of the SU from its raw information.
     * </p>
     * 
     * @param processFileName
     *            The file name of the process definition, as raw data read from the SU JBI descriptor. Not
     *            <code>null</code>.
     * @param versionStr
     *            The version of the process, as raw data read from the SU JBI descriptor. Not <code>null</code>.
     * @throws ProcessDefinitionDeclarationException
     *             A raw data of the SU JBI descriptor is invalid
     */
    private EmbeddedProcessDefinition readBpmnModel(final String processFileName, final String versionStr)
            throws ProcessDefinitionDeclarationException {

        assert processFileName != null;
        assert versionStr != null;

        if (processFileName.trim().isEmpty()) {
            throw new IncoherentProcessDefinitionDeclarationException(processFileName, versionStr);
        }

        final int version;
        if (versionStr.trim().isEmpty()) {
            throw new IncoherentProcessDefinitionDeclarationException(processFileName, versionStr);
        } else {
            try {
                version = Integer.parseInt(versionStr);
            } catch (final NumberFormatException e) {
                throw new InvalidVersionDeclaredException(processFileName, versionStr);
            }
        }
        
        // Check that the BPMN file has a suffix expected by Flowable, otherwise its deployment will be skiped
        boolean correctlySuffixed = false;
        for (final String suffix : ResourceNameUtil.BPMN_RESOURCE_SUFFIXES) {
            if (processFileName.endsWith(suffix)) {
                correctlySuffixed = true;
                break;
            }
        }
        if (!correctlySuffixed) {
            throw new InvalidSuffixForProcessFileException(processFileName);
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
